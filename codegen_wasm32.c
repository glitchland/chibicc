// codegen_wasm32.c - WebAssembly code generator for chibicc
//
// This file implements a WebAssembly (WASM32) backend for chibicc.
// It generates WASM binary format targeting the wasm32-wasi ABI.
//
// =============================================================================
// ARCHITECTURE OVERVIEW
// =============================================================================
//
// WebAssembly is a stack-based virtual machine with these key characteristics:
//   - Values are pushed/popped from an implicit operand stack
//   - Linear memory is a flat byte array accessed via load/store instructions
//   - Functions have typed signatures and local variables
//   - Control flow uses structured constructs (block, loop, if, br, br_if)
//
// Memory Layout (wasm32-wasi convention):
//   0x00000000 - 0x00000FFF : Reserved/null guard page (4KB)
//   0x00001000 - heap_base  : Static data (.data, .rodata, .bss)
//   heap_base  - stack_base : Heap (grows up)
//   stack_base - 0x0000FFFF : Stack (grows down from initial 64KB)
//
// Calling Convention (wasm32):
//   - Arguments passed on WASM operand stack (not linear memory stack)
//   - Return value on WASM operand stack
//   - Structs > 8 bytes: caller provides pointer as first arg
//   - Local variables stored in WASM locals OR linear memory stack frame
//
// =============================================================================

#include "chibicc.h"

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

// =============================================================================
// WASM Binary Format Constants
// =============================================================================

// WASM Opcodes
typedef enum {
    // Control flow
    OP_UNREACHABLE   = 0x00,
    OP_NOP           = 0x01,
    OP_BLOCK         = 0x02,
    OP_LOOP          = 0x03,
    OP_IF            = 0x04,
    OP_ELSE          = 0x05,
    OP_END           = 0x0B,
    OP_BR            = 0x0C,
    OP_BR_IF         = 0x0D,
    OP_BR_TABLE      = 0x0E,
    OP_RETURN        = 0x0F,
    OP_CALL          = 0x10,
    OP_CALL_INDIRECT = 0x11,

    // Parametric
    OP_DROP          = 0x1A,
    OP_SELECT        = 0x1B,

    // Variable access
    OP_LOCAL_GET     = 0x20,
    OP_LOCAL_SET     = 0x21,
    OP_LOCAL_TEE     = 0x22,
    OP_GLOBAL_GET    = 0x23,
    OP_GLOBAL_SET    = 0x24,

    // Memory loads
    OP_I32_LOAD      = 0x28,
    OP_I64_LOAD      = 0x29,
    OP_F32_LOAD      = 0x2A,
    OP_F64_LOAD      = 0x2B,
    OP_I32_LOAD8_S   = 0x2C,
    OP_I32_LOAD8_U   = 0x2D,
    OP_I32_LOAD16_S  = 0x2E,
    OP_I32_LOAD16_U  = 0x2F,
    OP_I64_LOAD8_S   = 0x30,
    OP_I64_LOAD8_U   = 0x31,
    OP_I64_LOAD16_S  = 0x32,
    OP_I64_LOAD16_U  = 0x33,
    OP_I64_LOAD32_S  = 0x34,
    OP_I64_LOAD32_U  = 0x35,

    // Memory stores
    OP_I32_STORE     = 0x36,
    OP_I64_STORE     = 0x37,
    OP_F32_STORE     = 0x38,
    OP_F64_STORE     = 0x39,
    OP_I32_STORE8    = 0x3A,
    OP_I32_STORE16   = 0x3B,
    OP_I64_STORE8    = 0x3C,
    OP_I64_STORE16   = 0x3D,
    OP_I64_STORE32   = 0x3E,

    OP_MEMORY_SIZE   = 0x3F,
    OP_MEMORY_GROW   = 0x40,

    // Constants
    OP_I32_CONST     = 0x41,
    OP_I64_CONST     = 0x42,
    OP_F32_CONST     = 0x43,
    OP_F64_CONST     = 0x44,

    // i32 comparisons
    OP_I32_EQZ       = 0x45,
    OP_I32_EQ        = 0x46,
    OP_I32_NE        = 0x47,
    OP_I32_LT_S      = 0x48,
    OP_I32_LT_U      = 0x49,
    OP_I32_GT_S      = 0x4A,
    OP_I32_GT_U      = 0x4B,
    OP_I32_LE_S      = 0x4C,
    OP_I32_LE_U      = 0x4D,
    OP_I32_GE_S      = 0x4E,
    OP_I32_GE_U      = 0x4F,

    // i64 comparisons
    OP_I64_EQZ       = 0x50,
    OP_I64_EQ        = 0x51,
    OP_I64_NE        = 0x52,
    OP_I64_LT_S      = 0x53,
    OP_I64_LT_U      = 0x54,
    OP_I64_GT_S      = 0x55,
    OP_I64_GT_U      = 0x56,
    OP_I64_LE_S      = 0x57,
    OP_I64_LE_U      = 0x58,
    OP_I64_GE_S      = 0x59,
    OP_I64_GE_U      = 0x5A,

    // f32 comparisons
    OP_F32_EQ        = 0x5B,
    OP_F32_NE        = 0x5C,
    OP_F32_LT        = 0x5D,
    OP_F32_GT        = 0x5E,
    OP_F32_LE        = 0x5F,
    OP_F32_GE        = 0x60,

    // f64 comparisons
    OP_F64_EQ        = 0x61,
    OP_F64_NE        = 0x62,
    OP_F64_LT        = 0x63,
    OP_F64_GT        = 0x64,
    OP_F64_LE        = 0x65,
    OP_F64_GE        = 0x66,

    // i32 arithmetic
    OP_I32_CLZ       = 0x67,
    OP_I32_CTZ       = 0x68,
    OP_I32_POPCNT    = 0x69,
    OP_I32_ADD       = 0x6A,
    OP_I32_SUB       = 0x6B,
    OP_I32_MUL       = 0x6C,
    OP_I32_DIV_S     = 0x6D,
    OP_I32_DIV_U     = 0x6E,
    OP_I32_REM_S     = 0x6F,
    OP_I32_REM_U     = 0x70,
    OP_I32_AND       = 0x71,
    OP_I32_OR        = 0x72,
    OP_I32_XOR       = 0x73,
    OP_I32_SHL       = 0x74,
    OP_I32_SHR_S     = 0x75,
    OP_I32_SHR_U     = 0x76,
    OP_I32_ROTL      = 0x77,
    OP_I32_ROTR      = 0x78,

    // i64 arithmetic
    OP_I64_CLZ       = 0x79,
    OP_I64_CTZ       = 0x7A,
    OP_I64_POPCNT    = 0x7B,
    OP_I64_ADD       = 0x7C,
    OP_I64_SUB       = 0x7D,
    OP_I64_MUL       = 0x7E,
    OP_I64_DIV_S     = 0x7F,
    OP_I64_DIV_U     = 0x80,
    OP_I64_REM_S     = 0x81,
    OP_I64_REM_U     = 0x82,
    OP_I64_AND       = 0x83,
    OP_I64_OR        = 0x84,
    OP_I64_XOR       = 0x85,
    OP_I64_SHL       = 0x86,
    OP_I64_SHR_S     = 0x87,
    OP_I64_SHR_U     = 0x88,
    OP_I64_ROTL      = 0x89,
    OP_I64_ROTR      = 0x8A,

    // f32 arithmetic
    OP_F32_ABS       = 0x8B,
    OP_F32_NEG       = 0x8C,
    OP_F32_CEIL      = 0x8D,
    OP_F32_FLOOR     = 0x8E,
    OP_F32_TRUNC     = 0x8F,
    OP_F32_NEAREST   = 0x90,
    OP_F32_SQRT      = 0x91,
    OP_F32_ADD       = 0x92,
    OP_F32_SUB       = 0x93,
    OP_F32_MUL       = 0x94,
    OP_F32_DIV       = 0x95,
    OP_F32_MIN       = 0x96,
    OP_F32_MAX       = 0x97,
    OP_F32_COPYSIGN  = 0x98,

    // f64 arithmetic
    OP_F64_ABS       = 0x99,
    OP_F64_NEG       = 0x9A,
    OP_F64_CEIL      = 0x9B,
    OP_F64_FLOOR     = 0x9C,
    OP_F64_TRUNC     = 0x9D,
    OP_F64_NEAREST   = 0x9E,
    OP_F64_SQRT      = 0x9F,
    OP_F64_ADD       = 0xA0,
    OP_F64_SUB       = 0xA1,
    OP_F64_MUL       = 0xA2,
    OP_F64_DIV       = 0xA3,
    OP_F64_MIN       = 0xA4,
    OP_F64_MAX       = 0xA5,
    OP_F64_COPYSIGN  = 0xA6,

    // Conversions
    OP_I32_WRAP_I64       = 0xA7,
    OP_I32_TRUNC_F32_S    = 0xA8,
    OP_I32_TRUNC_F32_U    = 0xA9,
    OP_I32_TRUNC_F64_S    = 0xAA,
    OP_I32_TRUNC_F64_U    = 0xAB,
    OP_I64_EXTEND_I32_S   = 0xAC,
    OP_I64_EXTEND_I32_U   = 0xAD,
    OP_I64_TRUNC_F32_S    = 0xAE,
    OP_I64_TRUNC_F32_U    = 0xAF,
    OP_I64_TRUNC_F64_S    = 0xB0,
    OP_I64_TRUNC_F64_U    = 0xB1,
    OP_F32_CONVERT_I32_S  = 0xB2,
    OP_F32_CONVERT_I32_U  = 0xB3,
    OP_F32_CONVERT_I64_S  = 0xB4,
    OP_F32_CONVERT_I64_U  = 0xB5,
    OP_F32_DEMOTE_F64     = 0xB6,
    OP_F64_CONVERT_I32_S  = 0xB7,
    OP_F64_CONVERT_I32_U  = 0xB8,
    OP_F64_CONVERT_I64_S  = 0xB9,
    OP_F64_CONVERT_I64_U  = 0xBA,
    OP_F64_PROMOTE_F32    = 0xBB,
    OP_I32_REINTERPRET_F32 = 0xBC,
    OP_I64_REINTERPRET_F64 = 0xBD,
    OP_F32_REINTERPRET_I32 = 0xBE,
    OP_F64_REINTERPRET_I64 = 0xBF,

    // Sign extension (WASM 1.0 with sign-extension proposal)
    OP_I32_EXTEND8_S  = 0xC0,
    OP_I32_EXTEND16_S = 0xC1,
    OP_I64_EXTEND8_S  = 0xC2,
    OP_I64_EXTEND16_S = 0xC3,
    OP_I64_EXTEND32_S = 0xC4,
} WasmOp;

// WASM value types
typedef enum {
    TYPE_I32   = 0x7F,
    TYPE_I64   = 0x7E,
    TYPE_F32   = 0x7D,
    TYPE_F64   = 0x7C,
    TYPE_VOID  = 0x40,  // Block type with no result
} WasmType;

// WASM section IDs
typedef enum {
    SECTION_CUSTOM   = 0,
    SECTION_TYPE     = 1,
    SECTION_IMPORT   = 2,
    SECTION_FUNCTION = 3,
    SECTION_TABLE    = 4,
    SECTION_MEMORY   = 5,
    SECTION_GLOBAL   = 6,
    SECTION_EXPORT   = 7,
    SECTION_START    = 8,
    SECTION_ELEMENT  = 9,
    SECTION_CODE     = 10,
    SECTION_DATA     = 11,
} WasmSection;

// =============================================================================
// Buffer - Dynamic byte array for building WASM binary
// =============================================================================

typedef struct {
    uint8_t *data;
    size_t len;
    size_t cap;
} Buffer;

static Buffer *buf_new(void) {
    Buffer *b = calloc(1, sizeof(Buffer));
    b->cap = 1024;
    b->data = malloc(b->cap);
    return b;
}

static void buf_free(Buffer *b) {
    if (b) {
        free(b->data);
        free(b);
    }
}

static void buf_push(Buffer *b, uint8_t byte) {
    if (b->len >= b->cap) {
        b->cap *= 2;
        b->data = realloc(b->data, b->cap);
    }
    b->data[b->len++] = byte;
}

static void buf_push_bytes(Buffer *b, const void *data, size_t len) {
    for (size_t i = 0; i < len; i++)
        buf_push(b, ((uint8_t *)data)[i]);
}

// LEB128 unsigned encoding
static void buf_push_uleb128(Buffer *b, uint64_t val) {
    do {
        uint8_t byte = val & 0x7F;
        val >>= 7;
        if (val != 0)
            byte |= 0x80;
        buf_push(b, byte);
    } while (val != 0);
}

// LEB128 signed encoding
static void buf_push_sleb128(Buffer *b, int64_t val) {
    int more = 1;
    while (more) {
        uint8_t byte = val & 0x7F;
        val >>= 7;
        int sign_bit = byte & 0x40;
        if ((val == 0 && !sign_bit) || (val == -1 && sign_bit))
            more = 0;
        else
            byte |= 0x80;
        buf_push(b, byte);
    }
}

// Length-prefixed string
static void buf_push_name(Buffer *b, const char *name) {
    size_t len = strlen(name);
    buf_push_uleb128(b, len);
    buf_push_bytes(b, name, len);
}

// =============================================================================
// Symbol Table - Maps names to indices
// =============================================================================

typedef struct SymEntry {
    char *name;
    int index;
    int type_idx;       // For functions: their type index
    bool is_import;
    struct SymEntry *next;
} SymEntry;

typedef struct {
    SymEntry *head;
    int count;
} SymTable;

static SymTable *symtab_new(void) {
    return calloc(1, sizeof(SymTable));
}

static void symtab_add(SymTable *t, const char *name, int index, int type_idx, bool is_import) {
    SymEntry *e = calloc(1, sizeof(SymEntry));
    e->name = strdup(name);
    e->index = index;
    e->type_idx = type_idx;
    e->is_import = is_import;
    e->next = t->head;
    t->head = e;
    t->count++;
}

static SymEntry *symtab_find(SymTable *t, const char *name) {
    for (SymEntry *e = t->head; e; e = e->next)
        if (strcmp(e->name, name) == 0)
            return e;
    return NULL;
}

// ==============================================================================
// Utilities
// ==============================================================================
// Returns true if this expression kind leaves a value on the stack
static bool expr_has_value(Node *node) {
    if (!node) return false;
    if (!node->ty) return false;
    if (node->ty->kind == TY_VOID) return false;

    switch (node->kind) {
    case ND_NULL_EXPR:
    case ND_MEMZERO:
    case ND_STMT_EXPR:   // wrapper around statements; gen_expr just calls gen_stmt
        return false;

    // If you later decide ND_ASSIGN should *not* leave a value, add:
    // case ND_ASSIGN:
    //     return false;

    default:
        return true;
    }
}

// =============================================================================
// WASM Module Builder
// =============================================================================

typedef struct {
    Buffer *types;
    Buffer *imports;
    Buffer *functions;
    Buffer *tables;
    Buffer *memory;
    Buffer *globals;
    Buffer *exports;
    Buffer *start;
    Buffer *elements;
    Buffer *code;
    Buffer *data;

    int num_types;
    int num_imports;
    int num_functions;
    int num_globals;
    int num_exports;
    int num_data_segments;

    // Symbol tables
    SymTable *func_syms;
    SymTable *global_syms;

    // Memory layout
    int data_offset;        // Next available offset in data section
    int stack_pointer_global;
    int heap_base_global;

    // For indirect calls (function pointers)
    Buffer *func_table;
    int num_table_entries;
} WasmModule;

static WasmModule *module_new(void) {
    WasmModule *m = calloc(1, sizeof(WasmModule));
    m->types = buf_new();
    m->imports = buf_new();
    m->functions = buf_new();
    m->tables = buf_new();
    m->memory = buf_new();
    m->globals = buf_new();
    m->exports = buf_new();
    m->start = buf_new();
    m->elements = buf_new();
    m->code = buf_new();
    m->data = buf_new();
    m->func_table = buf_new();

    m->func_syms = symtab_new();
    m->global_syms = symtab_new();

    // Reserve space for null pointer and initial data
    m->data_offset = 1024;  // Start data after 1KB guard

    return m;
}

// Add a function type signature
static int module_add_type(WasmModule *m, WasmType *params, int num_params,
                           WasmType *results, int num_results) {
    // Check for existing identical type
    // (optimization: reuse type indices)
    buf_push(m->types, 0x60);  // Function type marker
    buf_push_uleb128(m->types, num_params);
    for (int i = 0; i < num_params; i++)
        buf_push(m->types, params[i]);
    buf_push_uleb128(m->types, num_results);
    for (int i = 0; i < num_results; i++)
        buf_push(m->types, results[i]);
    return m->num_types++;
}

// Add an imported function
static int module_add_import_func(WasmModule *m, const char *module_name,
                                  const char *field_name, int type_idx) {
    buf_push_name(m->imports, module_name);
    buf_push_name(m->imports, field_name);
    buf_push(m->imports, 0x00);  // Import kind: function
    buf_push_uleb128(m->imports, type_idx);

    int idx = m->num_imports++;
    symtab_add(m->func_syms, field_name, idx, type_idx, true);
    return idx;
}

// Add a function (declaration only, body added separately)
static int module_add_function(WasmModule *m, const char *name, int type_idx) {
    buf_push_uleb128(m->functions, type_idx);
    int idx = m->num_imports + m->num_functions++;
    symtab_add(m->func_syms, name, idx, type_idx, false);
    return idx;
}

// Add an export
static void module_add_export(WasmModule *m, const char *name, int kind, int index) {
    buf_push_name(m->exports, name);
    buf_push(m->exports, kind);  // 0=func, 1=table, 2=memory, 3=global
    buf_push_uleb128(m->exports, index);
    m->num_exports++;
}

// Add a mutable global variable
static int module_add_global_i32(WasmModule *m, const char *name, int32_t init, bool mutable) {
    buf_push(m->globals, TYPE_I32);
    buf_push(m->globals, mutable ? 1 : 0);
    buf_push(m->globals, OP_I32_CONST);
    buf_push_sleb128(m->globals, init);
    buf_push(m->globals, OP_END);

    int idx = m->num_globals++;
    if (name)
        symtab_add(m->global_syms, name, idx, -1, false);
    return idx;
}

// Add initialized data segment
static int module_add_data(WasmModule *m, const void *data, size_t len, int align) {
    // Align the offset
    m->data_offset = (m->data_offset + align - 1) & ~(align - 1);

    buf_push(m->data, 0x00);  // Active segment, memory 0
    buf_push(m->data, OP_I32_CONST);
    buf_push_sleb128(m->data, m->data_offset);
    buf_push(m->data, OP_END);
    buf_push_uleb128(m->data, len);
    buf_push_bytes(m->data, data, len);

    int offset = m->data_offset;
    m->data_offset += len;
    m->num_data_segments++;

    return offset;
}

// Reserve BSS space (uninitialized data)
static int module_reserve_bss(WasmModule *m, size_t len, int align) {
    m->data_offset = (m->data_offset + align - 1) & ~(align - 1);
    int offset = m->data_offset;
    m->data_offset += len;
    return offset;
}

// =============================================================================
// Function Builder - Generates bytecode for a single function
// =============================================================================

typedef struct {
    Buffer *code;
    int num_params;
    int num_locals_i32;
    int num_locals_i64;
    int num_locals_f32;
    int num_locals_f64;
    int label_depth;        // Current nesting depth for br targets
    int frame_size;         // Stack frame size for this function

    // Local variable mapping
    // Params come first, then i32 locals, then i64, f32, f64
    int local_base_i32;
    int local_base_i64;
    int local_base_f32;
    int local_base_f64;
} FuncBuilder;

static FuncBuilder *func_new(int num_params) {
    FuncBuilder *f = calloc(1, sizeof(FuncBuilder));
    f->code = buf_new();
    f->num_params = num_params;
    f->local_base_i32 = num_params;
    return f;
}

static void func_free(FuncBuilder *f) {
    if (f) {
        buf_free(f->code);
        free(f);
    }
}

// Declare local variables (called after counting all locals)
static void func_emit_locals(FuncBuilder *f) {
    int num_groups = (f->num_locals_i32 > 0) + (f->num_locals_i64 > 0) +
                     (f->num_locals_f32 > 0) + (f->num_locals_f64 > 0);

    buf_push_uleb128(f->code, num_groups);

    if (f->num_locals_i32 > 0) {
        buf_push_uleb128(f->code, f->num_locals_i32);
        buf_push(f->code, TYPE_I32);
    }
    if (f->num_locals_i64 > 0) {
        buf_push_uleb128(f->code, f->num_locals_i64);
        buf_push(f->code, TYPE_I64);
    }
    if (f->num_locals_f32 > 0) {
        buf_push_uleb128(f->code, f->num_locals_f32);
        buf_push(f->code, TYPE_F32);
    }
    if (f->num_locals_f64 > 0) {
        buf_push_uleb128(f->code, f->num_locals_f64);
        buf_push(f->code, TYPE_F64);
    }

    // Update base indices
    f->local_base_i64 = f->local_base_i32 + f->num_locals_i32;
    f->local_base_f32 = f->local_base_i64 + f->num_locals_i64;
    f->local_base_f64 = f->local_base_f32 + f->num_locals_f32;
}

// =============================================================================
// Opcode Emission Helpers
// =============================================================================

static void emit_op(FuncBuilder *f, WasmOp op) {
    buf_push(f->code, op);
}

static void emit_op_u(FuncBuilder *f, WasmOp op, uint64_t val) {
    buf_push(f->code, op);
    buf_push_uleb128(f->code, val);
}

static void emit_op_s(FuncBuilder *f, WasmOp op, int64_t val) {
    buf_push(f->code, op);
    buf_push_sleb128(f->code, val);
}

static void emit_mem_op(FuncBuilder *f, WasmOp op, int align_log2, int offset) {
    buf_push(f->code, op);
    buf_push_uleb128(f->code, align_log2);
    buf_push_uleb128(f->code, offset);
}

// Constants
static void emit_i32_const(FuncBuilder *f, int32_t val) {
    emit_op_s(f, OP_I32_CONST, val);
}

static void emit_i64_const(FuncBuilder *f, int64_t val) {
    emit_op_s(f, OP_I64_CONST, val);
}

static void emit_f32_const(FuncBuilder *f, float val) {
    buf_push(f->code, OP_F32_CONST);
    buf_push_bytes(f->code, &val, 4);
}

static void emit_f64_const(FuncBuilder *f, double val) {
    buf_push(f->code, OP_F64_CONST);
    buf_push_bytes(f->code, &val, 8);
}

// Local/Global access
static void emit_local_get(FuncBuilder *f, int index) {
    emit_op_u(f, OP_LOCAL_GET, index);
}

static void emit_local_set(FuncBuilder *f, int index) {
    emit_op_u(f, OP_LOCAL_SET, index);
}

static void emit_local_tee(FuncBuilder *f, int index) {
    emit_op_u(f, OP_LOCAL_TEE, index);
}

static void emit_global_get(FuncBuilder *f, int index) {
    emit_op_u(f, OP_GLOBAL_GET, index);
}

static void emit_global_set(FuncBuilder *f, int index) {
    emit_op_u(f, OP_GLOBAL_SET, index);
}

// Function calls
static void emit_call(FuncBuilder *f, int func_index) {
    emit_op_u(f, OP_CALL, func_index);
}

// Memory operations
static void emit_i32_load(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_LOAD, 2, offset);
}

static void emit_i64_load(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I64_LOAD, 3, offset);
}

static void emit_f32_load(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_F32_LOAD, 2, offset);
}

static void emit_f64_load(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_F64_LOAD, 3, offset);
}

static void emit_i32_load8_s(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_LOAD8_S, 0, offset);
}

static void emit_i32_load8_u(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_LOAD8_U, 0, offset);
}

static void emit_i32_load16_s(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_LOAD16_S, 1, offset);
}

static void emit_i32_load16_u(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_LOAD16_U, 1, offset);
}

static void emit_i32_store(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_STORE, 2, offset);
}

static void emit_i64_store(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I64_STORE, 3, offset);
}

static void emit_f32_store(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_F32_STORE, 2, offset);
}

static void emit_f64_store(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_F64_STORE, 3, offset);
}

static void emit_i32_store8(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_STORE8, 0, offset);
}

static void emit_i32_store16(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_STORE16, 1, offset);
}

// Control flow
static void emit_block(FuncBuilder *f, WasmType result) {
    emit_op(f, OP_BLOCK);
    buf_push(f->code, result);
    f->label_depth++;
}

static void emit_loop(FuncBuilder *f, WasmType result) {
    emit_op(f, OP_LOOP);
    buf_push(f->code, result);
    f->label_depth++;
}

static void emit_if(FuncBuilder *f, WasmType result) {
    emit_op(f, OP_IF);
    buf_push(f->code, result);
    f->label_depth++;
}

static void emit_else(FuncBuilder *f) {
    emit_op(f, OP_ELSE);
}

static void emit_end(FuncBuilder *f) {
    emit_op(f, OP_END);
    if (f->label_depth > 0)
        f->label_depth--;
}

static void emit_br(FuncBuilder *f, int depth) {
    emit_op_u(f, OP_BR, depth);
}

static void emit_br_if(FuncBuilder *f, int depth) {
    emit_op_u(f, OP_BR_IF, depth);
}

static void emit_return(FuncBuilder *f) {
    emit_op(f, OP_RETURN);
}

static void emit_drop(FuncBuilder *f) {
    printf("EMIT_DROP at code offset %zu\n", f->code->len);
    emit_op(f, OP_DROP);
}

static void emit_select(FuncBuilder *f) {
    emit_op(f, OP_SELECT);
}

// =============================================================================
// Code Generator State
// =============================================================================

static FILE *output_file;
static WasmModule *module;
static FuncBuilder *current_func;
static Obj *current_fn;

// For tracking control flow labels
typedef struct LabelInfo {
    char *name;
    int block_depth;    // Depth when this label's block was opened
    bool is_loop;       // true = loop (for continue), false = block (for break)
    struct LabelInfo *next;
} LabelInfo;

static LabelInfo *label_stack = NULL;

static void push_label(const char *name, int depth, bool is_loop) {
    LabelInfo *l = calloc(1, sizeof(LabelInfo));
    if (name)
        l->name = strdup(name);
    else
        l->name = NULL;
    l->block_depth = depth;
    l->is_loop = is_loop;
    l->next = label_stack;
    label_stack = l;
}

static void pop_label(void) {
    if (label_stack) {
        LabelInfo *l = label_stack;
        label_stack = l->next;
        free(l->name);
        free(l);
    }
}

static LabelInfo *find_label(const char *name) {
    for (LabelInfo *l = label_stack; l; l = l->next)
        if (l->name && strcmp(l->name, name) == 0)
            return l;
    return NULL;
}

// Break/continue target tracking - store absolute label depths
static int break_label_depth = -1;    // label_depth when break block was opened
static int continue_label_depth = -1; // label_depth when loop was opened

// =============================================================================
// Type Helpers
// =============================================================================

static WasmType get_wasm_type(Type *ty) {
    switch (ty->kind) {
    case TY_VOID:
        return TYPE_VOID;
    case TY_BOOL:
    case TY_CHAR:
    case TY_SHORT:
    case TY_INT:
    case TY_ENUM:
        return TYPE_I32;
    case TY_LONG:
        return TYPE_I64;  // wasm32: long is 32-bit, but we'll use i64 for compatibility
    case TY_FLOAT:
        return TYPE_F32;
    case TY_DOUBLE:
    case TY_LDOUBLE:
        return TYPE_F64;
    case TY_PTR:
    case TY_ARRAY:
    case TY_FUNC:
        return TYPE_I32;  // Pointers are 32-bit in wasm32
    case TY_STRUCT:
    case TY_UNION:
        return TYPE_I32;  // Structs passed by pointer
    default:
        return TYPE_I32;
    }
}

static bool is_i64_type(Type *ty) {
    return ty->kind == TY_LONG && ty->size == 8;
}

static bool is_float_type(Type *ty) {
    return ty->kind == TY_FLOAT;
}

static bool is_double_type(Type *ty) {
    return ty->kind == TY_DOUBLE || ty->kind == TY_LDOUBLE;
}

// =============================================================================
// Forward Declarations
// =============================================================================

static void gen_expr(Node *node);
static void gen_stmt(Node *node);
static void gen_addr(Node *node);

// =============================================================================
// Address Generation
// =============================================================================

// Generate code to push the address of a node onto the stack
static void gen_addr(Node *node) {
    switch (node->kind) {
    case ND_VAR: {
        Obj *var = node->var;

        if (var->is_local) {
            // Local variable: base pointer + offset
            // In WASM, we use a dedicated local as frame pointer
            // For simplicity, we'll use linear memory with stack pointer
            emit_global_get(current_func, module->stack_pointer_global);
            emit_i32_const(current_func, var->offset);
            emit_op(current_func, OP_I32_ADD);
        } else {
            // Global variable: use its data section offset
            SymEntry *sym = symtab_find(module->global_syms, var->name);
            if (sym) {
                emit_i32_const(current_func, sym->index);  // index stores memory offset
            } else {
                error_tok(node->tok, "undefined global variable: %s", var->name);
            }
        }
        return;
    }
    case ND_DEREF:
        gen_expr(node->lhs);
        return;

    case ND_COMMA:
        gen_expr(node->lhs);
        if (expr_has_value(node->lhs)) {
            emit_drop(current_func);
        }
        gen_addr(node->rhs);
        return;

    case ND_MEMBER:
        gen_addr(node->lhs);
        if (node->member->offset > 0) {
            emit_i32_const(current_func, node->member->offset);
            emit_op(current_func, OP_I32_ADD);
        }
        return;

    default:
        error_tok(node->tok, "not an lvalue");
    }
}

// =============================================================================
// Load/Store Operations
// =============================================================================

// Load a value from address on stack
static void load(Type *ty) {
    switch (ty->kind) {
    case TY_ARRAY:
    case TY_STRUCT:
    case TY_UNION:
    case TY_FUNC:
    case TY_VLA:
        // Don't load arrays/structs - keep the address
        return;

    case TY_FLOAT:
        emit_f32_load(current_func, 0);
        return;

    case TY_DOUBLE:
    case TY_LDOUBLE:
        emit_f64_load(current_func, 0);
        return;

    case TY_LONG:
        if (ty->size == 8) {
            emit_i64_load(current_func, 0);
            return;
        }
        // Fall through for 32-bit long
    }

    // Integer types
    if (ty->size == 1) {
        if (ty->is_unsigned)
            emit_i32_load8_u(current_func, 0);
        else
            emit_i32_load8_s(current_func, 0);
    } else if (ty->size == 2) {
        if (ty->is_unsigned)
            emit_i32_load16_u(current_func, 0);
        else
            emit_i32_load16_s(current_func, 0);
    } else {
        emit_i32_load(current_func, 0);
    }
}

// Store value to address
// Stack: [address, value] -> []
static void store(Type *ty) {
    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
        // For struct assignment, we need to copy bytes
        // This is complex - for now, emit a memcpy-like sequence
        // TODO: Implement proper struct copy
        error("struct assignment not yet implemented for WASM");
        return;

    case TY_FLOAT:
        emit_f32_store(current_func, 0);
        return;

    case TY_DOUBLE:
    case TY_LDOUBLE:
        emit_f64_store(current_func, 0);
        return;

    case TY_LONG:
        if (ty->size == 8) {
            emit_i64_store(current_func, 0);
            return;
        }
    }

    // Integer types
    if (ty->size == 1)
        emit_i32_store8(current_func, 0);
    else if (ty->size == 2)
        emit_i32_store16(current_func, 0);
    else
        emit_i32_store(current_func, 0);
}

// =============================================================================
// Type Casting
// =============================================================================

static void cast(Type *from, Type *to) {
    if (to->kind == TY_VOID)
        return;

    if (to->kind == TY_BOOL) {
        // Convert to bool: compare with zero
        switch (from->kind) {
        case TY_FLOAT:
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_NE);
            break;
        case TY_DOUBLE:
        case TY_LDOUBLE:
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_NE);
            break;
        case TY_LONG:
            if (from->size == 8) {
                emit_op(current_func, OP_I64_EQZ);
                emit_op(current_func, OP_I32_EQZ);
                break;
            }
            // Fall through
        default:
            emit_op(current_func, OP_I32_EQZ);
            emit_op(current_func, OP_I32_EQZ);
        }
        return;
    }

    WasmType wfrom = get_wasm_type(from);
    WasmType wto = get_wasm_type(to);

    if (wfrom == wto)
        return;

    // Integer to integer
    if (wfrom == TYPE_I32 && wto == TYPE_I64) {
        if (from->is_unsigned)
            emit_op(current_func, OP_I64_EXTEND_I32_U);
        else
            emit_op(current_func, OP_I64_EXTEND_I32_S);
    } else if (wfrom == TYPE_I64 && wto == TYPE_I32) {
        emit_op(current_func, OP_I32_WRAP_I64);
    }
    // Integer to float
    else if (wfrom == TYPE_I32 && wto == TYPE_F32) {
        if (from->is_unsigned)
            emit_op(current_func, OP_F32_CONVERT_I32_U);
        else
            emit_op(current_func, OP_F32_CONVERT_I32_S);
    } else if (wfrom == TYPE_I32 && wto == TYPE_F64) {
        if (from->is_unsigned)
            emit_op(current_func, OP_F64_CONVERT_I32_U);
        else
            emit_op(current_func, OP_F64_CONVERT_I32_S);
    } else if (wfrom == TYPE_I64 && wto == TYPE_F32) {
        if (from->is_unsigned)
            emit_op(current_func, OP_F32_CONVERT_I64_U);
        else
            emit_op(current_func, OP_F32_CONVERT_I64_S);
    } else if (wfrom == TYPE_I64 && wto == TYPE_F64) {
        if (from->is_unsigned)
            emit_op(current_func, OP_F64_CONVERT_I64_U);
        else
            emit_op(current_func, OP_F64_CONVERT_I64_S);
    }
    // Float to integer
    else if (wfrom == TYPE_F32 && wto == TYPE_I32) {
        if (to->is_unsigned)
            emit_op(current_func, OP_I32_TRUNC_F32_U);
        else
            emit_op(current_func, OP_I32_TRUNC_F32_S);
    } else if (wfrom == TYPE_F64 && wto == TYPE_I32) {
        if (to->is_unsigned)
            emit_op(current_func, OP_I32_TRUNC_F64_U);
        else
            emit_op(current_func, OP_I32_TRUNC_F64_S);
    } else if (wfrom == TYPE_F32 && wto == TYPE_I64) {
        if (to->is_unsigned)
            emit_op(current_func, OP_I64_TRUNC_F32_U);
        else
            emit_op(current_func, OP_I64_TRUNC_F32_S);
    } else if (wfrom == TYPE_F64 && wto == TYPE_I64) {
        if (to->is_unsigned)
            emit_op(current_func, OP_I64_TRUNC_F64_U);
        else
            emit_op(current_func, OP_I64_TRUNC_F64_S);
    }
    // Float to float
    else if (wfrom == TYPE_F32 && wto == TYPE_F64) {
        emit_op(current_func, OP_F64_PROMOTE_F32);
    } else if (wfrom == TYPE_F64 && wto == TYPE_F32) {
        emit_op(current_func, OP_F32_DEMOTE_F64);
    }
}

// =============================================================================
// Expression Code Generation
// =============================================================================

static void gen_expr(Node *node) {
    if (!node) {
        printf("gen_expr: NULL node!\n");
        return;
    }
    printf("gen_expr: kind=%d\n", node->kind);
    fflush(stdout);
    switch (node->kind) {
    // Handle the different chibicc node types
    case ND_NULL_EXPR:
        return;

    case ND_NUM:
        switch (node->ty->kind) {
        case TY_FLOAT:
            emit_f32_const(current_func, (float)node->fval);
            return;
        case TY_DOUBLE:
        case TY_LDOUBLE:
            emit_f64_const(current_func, (double)node->fval);
            return;
        case TY_LONG:
            if (node->ty->size == 8) {
                emit_i64_const(current_func, node->val);
                return;
            }
            // Fall through
        default:
            emit_i32_const(current_func, (int32_t)node->val);
            return;
        }

    case ND_NEG:
      gen_expr(node->lhs);
      switch (node->ty->kind) {
        case TY_FLOAT:
            emit_op(current_func, OP_F32_NEG);
            break;
        case TY_DOUBLE:
        case TY_LDOUBLE:
            emit_op(current_func, OP_F64_NEG);
            break;
        case TY_LONG:
            if (node->ty->size == 8) {
                // i64: use (0 - x) via temp local
                int temp = current_func->local_base_i32 + current_func->num_locals_i32 - 1;
                emit_op(current_func, OP_I64_CONST);
                buf_push_sleb128(current_func->code, 0);
                // Stack: [x, 0], need [0, x]
                // Actually easier: negate = subtract from 0
                // Rewrite: push to temp, push 0, get temp, sub
                // Simpler approach using i64.sub semantics:
                emit_i64_const(current_func, 0);
                emit_op(current_func, OP_I64_SUB);
                // That gives 0 - x only if we reorder. Let's use:
                // x is on stack. We need: 0 i64.const, swap, i64.sub
                // WASM has no swap. Use local:
                emit_local_set(current_func, temp);
                emit_i64_const(current_func, 0);
                emit_local_get(current_func, temp);
                emit_op(current_func, OP_I64_SUB);
                break;
            }
            // Fall through for 32-bit
        default: {
            // i32: 0 - x
            int temp = current_func->local_base_i32 + current_func->num_locals_i32 - 1;
            emit_local_set(current_func, temp);
            emit_i32_const(current_func, 0);
            emit_local_get(current_func, temp);
            emit_op(current_func, OP_I32_SUB);
            break;
        }
      }
      return;
      /*
        // Actually, let me redo this properly:
        if (node->ty->kind == TY_FLOAT || node->ty->kind == TY_DOUBLE || node->ty->kind == TY_LDOUBLE) {
            // Already handled above
        } else if (node->ty->kind == TY_LONG && node->ty->size == 8) {
            // For i64: compute 0 - x by first saving x, pushing 0, getting x, sub
            // Simpler: i64_const 0, local.tee temp, drop, gen_expr, local.get temp
            // Even simpler for neg: just use xor with -1 and add 1? No, that's complicated.
            // Let's use the approach: emit 0 first in a wrapper, or negate differently.
            // For i64, we can use: i64.const -1, i64.xor, i64.const 1, i64.add
            // But that's 2's complement and works. But simpler:
            // Actually, I already generated gen_expr above. Stack has value.
            // To negate: multiply by -1 or subtract from 0.
            // The issue is stack order. Let me just use a different approach for neg:
            // Regenerate properly:
        } else {
            // i32 negation: 0 - x
            // We generated x above, and pushed 0. Stack is [x, 0]. We want [0, x] for sub.
            // This is wrong. Let me fix the whole case:
        }
        // Let me rewrite the NEG case properly:
        return;  // Placeholder - will fix below
    */
    case ND_VAR:
        gen_addr(node);
        load(node->ty);
        return;

    case ND_MEMBER:
        gen_addr(node);
        load(node->ty);
        return;

    case ND_DEREF:
        gen_expr(node->lhs);
        load(node->ty);
        return;

    case ND_ADDR:
        gen_addr(node->lhs);
        return;

    case ND_ASSIGN:
        gen_addr(node->lhs);
        gen_expr(node->rhs);
        // Stack: [addr, value]
        // But store wants [addr, value] and we want to return value
        // So: tee into temp, store, get temp
        // For simplicity, let's just do: addr, value, store (value consumed)
        // But assignment expression should return the assigned value!
        // Use local.tee on value before store... but store consumes both.
        // Solution: dup the value before store
        // WASM doesn't have dup, so use local.tee
        // Actually, for now, let's just store and reload:
        store(node->ty);
        // To return value, we'd need to gen_addr again and load
        // For now, expressions like (a = b) won't work in larger expressions
        // TODO: Fix this properly with a temp local
        gen_addr(node->lhs);
        load(node->ty);
        return;

    case ND_COMMA:
        gen_expr(node->lhs);
        if (expr_has_value(node->lhs)) {
            emit_drop(current_func);
        }
        gen_expr(node->rhs);
        return;

    case ND_CAST:
        gen_expr(node->lhs);
        cast(node->lhs->ty, node->ty);
        return;

    case ND_NOT:
        gen_expr(node->lhs);
        switch (node->lhs->ty->kind) {
        case TY_FLOAT:
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_EQ);
            break;
        case TY_DOUBLE:
        case TY_LDOUBLE:
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_EQ);
            break;
        case TY_LONG:
            if (node->lhs->ty->size == 8) {
                emit_op(current_func, OP_I64_EQZ);
                break;
            }
            // Fall through
        default:
            emit_op(current_func, OP_I32_EQZ);
        }
        return;

    case ND_BITNOT:
        gen_expr(node->lhs);
        if (node->ty->kind == TY_LONG && node->ty->size == 8) {
            emit_i64_const(current_func, -1);
            emit_op(current_func, OP_I64_XOR);
        } else {
            emit_i32_const(current_func, -1);
            emit_op(current_func, OP_I32_XOR);
        }
        return;

    case ND_LOGAND: {
        // Short-circuit AND: if lhs is 0, result is 0; else evaluate rhs
        gen_expr(node->lhs);
        // Convert to bool
        if (is_float_type(node->lhs->ty)) {
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_NE);
        } else if (is_double_type(node->lhs->ty)) {
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_NE);
        } else {
            emit_op(current_func, OP_I32_EQZ);
            emit_op(current_func, OP_I32_EQZ);
        }

        emit_if(current_func, TYPE_I32);  // if lhs != 0
        gen_expr(node->rhs);
        // Convert rhs to bool
        if (is_float_type(node->rhs->ty)) {
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_NE);
        } else if (is_double_type(node->rhs->ty)) {
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_NE);
        } else {
            emit_op(current_func, OP_I32_EQZ);
            emit_op(current_func, OP_I32_EQZ);
        }
        emit_else(current_func);
        emit_i32_const(current_func, 0);
        emit_end(current_func);
        return;
    }

    case ND_LOGOR: {
        // Short-circuit OR
        gen_expr(node->lhs);
        if (is_float_type(node->lhs->ty)) {
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_NE);
        } else if (is_double_type(node->lhs->ty)) {
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_NE);
        } else {
            emit_op(current_func, OP_I32_EQZ);
            emit_op(current_func, OP_I32_EQZ);
        }

        emit_if(current_func, TYPE_I32);  // if lhs != 0
        emit_i32_const(current_func, 1);
        emit_else(current_func);
        gen_expr(node->rhs);
        if (is_float_type(node->rhs->ty)) {
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_NE);
        } else if (is_double_type(node->rhs->ty)) {
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_NE);
        } else {
            emit_op(current_func, OP_I32_EQZ);
            emit_op(current_func, OP_I32_EQZ);
        }
        emit_end(current_func);
        return;
    }

    case ND_COND: {
        // Ternary: cond ? then : else
        gen_expr(node->cond);
        // Convert condition to i32 bool
        if (is_float_type(node->cond->ty)) {
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_NE);
        } else if (is_double_type(node->cond->ty)) {
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_NE);
        }

        WasmType result_type = get_wasm_type(node->ty);
        emit_if(current_func, result_type);
        gen_expr(node->then);
        emit_else(current_func);
        gen_expr(node->els);
        emit_end(current_func);
        return;
    }

    case ND_FUNCALL: {
        // First, push all arguments
        int nargs = 0;
        for (Node *arg = node->args; arg; arg = arg->next) {
            gen_expr(arg);
            nargs++;
        }

        // Find the function
        Node *fn = node->lhs;
        if (fn->kind == ND_VAR) {
            SymEntry *sym = symtab_find(module->func_syms, fn->var->name);
            if (sym) {
                emit_call(current_func, sym->index);
            } else {
                // Don't crash - just print warning for now
                printf("Warning: undefined function: %s\n", fn->var->name);
                // TODO: Uncomment this: error_tok(node->tok, "undefined function: %s", fn->var->name);
                // Push a dummy return value
                emit_i32_const(current_func, 0);
            }
        } else {
            // Function pointer call - indirect call
            gen_expr(fn);  // Get function pointer (table index)
            // TODO: Implement call_indirect properly
            error_tok(node->tok, "indirect function calls not yet supported");
        }
        return;
    }

    case ND_STMT_EXPR:
        for (Node *n = node->body; n; n = n->next)
            gen_stmt(n);
        return;

    case ND_MEMZERO: {
        // Zero out memory for a variable
        // Don't call gen_addr(node) - node is ND_MEMZERO, not an lvalue
        // Instead, compute the address of node->var directly

        int size = node->var->ty->size;
        if (size <= 32) {
            for (int i = 0; i < size; i += 4) {
                emit_global_get(current_func, module->stack_pointer_global);
                emit_i32_const(current_func, node->var->offset + i);
                emit_op(current_func, OP_I32_ADD);
                emit_i32_const(current_func, 0);
                if (i + 4 <= size)
                    emit_i32_store(current_func, 0);
                else if (size - i >= 2) {
                    emit_i32_store16(current_func, 0);
                    // Handle remaining byte if needed
                } else {
                    emit_i32_store8(current_func, 0);
                }
            }
        } else {
            // TODO: Use memory.fill or a loop for larger sizes
            error_tok(node->tok, "large memzero not yet implemented");
        }
        return;
    }
    default:
        break;
    }

    // Binary operations
    Type *ty = node->lhs->ty;
    bool is_long = ty->kind == TY_LONG && ty->size == 8;
    bool is_float = ty->kind == TY_FLOAT;
    bool is_double = ty->kind == TY_DOUBLE || ty->kind == TY_LDOUBLE;

    gen_expr(node->lhs);
    gen_expr(node->rhs);

    switch (node->kind) {
    case ND_ADD:
        if (is_float) emit_op(current_func, OP_F32_ADD);
        else if (is_double) emit_op(current_func, OP_F64_ADD);
        else if (is_long) emit_op(current_func, OP_I64_ADD);
        else emit_op(current_func, OP_I32_ADD);
        return;

    case ND_SUB:
        if (is_float) emit_op(current_func, OP_F32_SUB);
        else if (is_double) emit_op(current_func, OP_F64_SUB);
        else if (is_long) emit_op(current_func, OP_I64_SUB);
        else emit_op(current_func, OP_I32_SUB);
        return;

    case ND_MUL:
        if (is_float) emit_op(current_func, OP_F32_MUL);
        else if (is_double) emit_op(current_func, OP_F64_MUL);
        else if (is_long) emit_op(current_func, OP_I64_MUL);
        else emit_op(current_func, OP_I32_MUL);
        return;

    case ND_DIV:
        if (is_float) emit_op(current_func, OP_F32_DIV);
        else if (is_double) emit_op(current_func, OP_F64_DIV);
        else if (is_long) {
            if (ty->is_unsigned) emit_op(current_func, OP_I64_DIV_U);
            else emit_op(current_func, OP_I64_DIV_S);
        } else {
            if (ty->is_unsigned) emit_op(current_func, OP_I32_DIV_U);
            else emit_op(current_func, OP_I32_DIV_S);
        }
        return;

    case ND_MOD:
        if (is_long) {
            if (ty->is_unsigned) emit_op(current_func, OP_I64_REM_U);
            else emit_op(current_func, OP_I64_REM_S);
        } else {
            if (ty->is_unsigned) emit_op(current_func, OP_I32_REM_U);
            else emit_op(current_func, OP_I32_REM_S);
        }
        return;

    case ND_BITAND:
        if (is_long) emit_op(current_func, OP_I64_AND);
        else emit_op(current_func, OP_I32_AND);
        return;

    case ND_BITOR:
        if (is_long) emit_op(current_func, OP_I64_OR);
        else emit_op(current_func, OP_I32_OR);
        return;

    case ND_BITXOR:
        if (is_long) emit_op(current_func, OP_I64_XOR);
        else emit_op(current_func, OP_I32_XOR);
        return;

    case ND_SHL:
        if (is_long) emit_op(current_func, OP_I64_SHL);
        else emit_op(current_func, OP_I32_SHL);
        return;

    case ND_SHR:
        if (is_long) {
            if (ty->is_unsigned) emit_op(current_func, OP_I64_SHR_U);
            else emit_op(current_func, OP_I64_SHR_S);
        } else {
            if (ty->is_unsigned) emit_op(current_func, OP_I32_SHR_U);
            else emit_op(current_func, OP_I32_SHR_S);
        }
        return;

    case ND_EQ:
        if (is_float) emit_op(current_func, OP_F32_EQ);
        else if (is_double) emit_op(current_func, OP_F64_EQ);
        else if (is_long) emit_op(current_func, OP_I64_EQ);
        else emit_op(current_func, OP_I32_EQ);
        return;

    case ND_NE:
        if (is_float) emit_op(current_func, OP_F32_NE);
        else if (is_double) emit_op(current_func, OP_F64_NE);
        else if (is_long) emit_op(current_func, OP_I64_NE);
        else emit_op(current_func, OP_I32_NE);
        return;

    case ND_LT:
        if (is_float) emit_op(current_func, OP_F32_LT);
        else if (is_double) emit_op(current_func, OP_F64_LT);
        else if (is_long) {
            if (ty->is_unsigned) emit_op(current_func, OP_I64_LT_U);
            else emit_op(current_func, OP_I64_LT_S);
        } else {
            if (ty->is_unsigned) emit_op(current_func, OP_I32_LT_U);
            else emit_op(current_func, OP_I32_LT_S);
        }
        return;

    case ND_LE:
        if (is_float) emit_op(current_func, OP_F32_LE);
        else if (is_double) emit_op(current_func, OP_F64_LE);
        else if (is_long) {
            if (ty->is_unsigned) emit_op(current_func, OP_I64_LE_U);
            else emit_op(current_func, OP_I64_LE_S);
        } else {
            if (ty->is_unsigned) emit_op(current_func, OP_I32_LE_U);
            else emit_op(current_func, OP_I32_LE_S);
        }
        return;

    default:
        error_tok(node->tok, "invalid expression");
    }
}

// =============================================================================
// Statement Code Generation
// =============================================================================

static void gen_stmt(Node *node) {
    if (!node) {
        printf("gen_stmt: NULL node!\n");
        return;
    }
    printf("gen_stmt: kind=%d\n", node->kind);
    fflush(stdout);

    switch (node->kind) {
    case ND_IF: {
        gen_expr(node->cond);
        // Ensure condition is i32
        if (is_float_type(node->cond->ty)) {
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_NE);
        } else if (is_double_type(node->cond->ty)) {
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_NE);
        }

        emit_if(current_func, TYPE_VOID);
        gen_stmt(node->then);
        if (node->els) {
            emit_else(current_func);
            gen_stmt(node->els);
        }
        emit_end(current_func);
        return;
    }

    case ND_FOR: {
        // for (init; cond; inc) body
        // WASM structure:
        //   init
        //   block (break target)
        //     loop (continue target)
        //       if (!cond) br to block
        //       body
        //       inc
        //       br to loop
        //     end loop
        //   end block

        if (node->init)
            gen_stmt(node->init);

        emit_block(current_func, TYPE_VOID);  // break target
        int my_break_label_depth = current_func->label_depth;  // Save depth after block

        emit_loop(current_func, TYPE_VOID);   // continue target
        int my_continue_label_depth = current_func->label_depth;  // Save depth after loop

        // Save old break/continue depths
        int old_break = break_label_depth;
        int old_continue = continue_label_depth;
        break_label_depth = my_break_label_depth;
        continue_label_depth = my_continue_label_depth;

        if (node->cond) {
            gen_expr(node->cond);
            if (is_float_type(node->cond->ty)) {
                emit_f32_const(current_func, 0.0f);
                emit_op(current_func, OP_F32_EQ);
            } else if (is_double_type(node->cond->ty)) {
                emit_f64_const(current_func, 0.0);
                emit_op(current_func, OP_F64_EQ);
            } else {
                emit_op(current_func, OP_I32_EQZ);
            }
            // Break if cond is false - branch to the block (break target)
            // Distance = current depth - break target depth
            emit_br_if(current_func, current_func->label_depth - break_label_depth);
        }

        gen_stmt(node->then);

        if (node->inc) {
            gen_expr(node->inc);
            if (node->inc->ty && node->inc->ty->kind != TY_VOID)
                emit_drop(current_func);
        }

        // Continue loop - branch to the loop
        emit_br(current_func, current_func->label_depth - continue_label_depth);

        emit_end(current_func);    // End loop
        emit_end(current_func);    // End block

        break_label_depth = old_break;
        continue_label_depth = old_continue;
        return;
    }

    case ND_DO: {
        // do body while (cond)
        emit_block(current_func, TYPE_VOID);  // break target
        int my_break_label_depth = current_func->label_depth;

        emit_loop(current_func, TYPE_VOID);   // continue/loop target
        int my_continue_label_depth = current_func->label_depth;

        int old_break = break_label_depth;
        int old_continue = continue_label_depth;
        break_label_depth = my_break_label_depth;
        continue_label_depth = my_continue_label_depth;

        gen_stmt(node->then);

        gen_expr(node->cond);
        if (is_float_type(node->cond->ty)) {
            emit_f32_const(current_func, 0.0f);
            emit_op(current_func, OP_F32_NE);
        } else if (is_double_type(node->cond->ty)) {
            emit_f64_const(current_func, 0.0);
            emit_op(current_func, OP_F64_NE);
        }
        // Continue if cond is true
        emit_br_if(current_func, current_func->label_depth - continue_label_depth);

        emit_end(current_func);  // End loop
        emit_end(current_func);  // End block

        break_label_depth = old_break;
        continue_label_depth = old_continue;
        return;
    }

    case ND_SWITCH: {
        gen_expr(node->cond);
        // Store condition in a temp local for comparisons
        int cond_local = current_func->local_base_i32 + current_func->num_locals_i32 - 1;
        emit_local_set(current_func, cond_local);

        emit_block(current_func, TYPE_VOID);  // break target
        int my_break_label_depth = current_func->label_depth;

        int old_break = break_label_depth;
        break_label_depth = my_break_label_depth;

        // Generate case comparisons
        for (Node *n = node->case_next; n; n = n->case_next) {
            emit_local_get(current_func, cond_local);
            emit_i32_const(current_func, (int32_t)n->begin);
            emit_op(current_func, OP_I32_EQ);

            emit_if(current_func, TYPE_VOID);
            gen_stmt(n->lhs);
            emit_end(current_func);
        }

        if (node->default_case)
            gen_stmt(node->default_case->lhs);

        emit_end(current_func);  // End break block

        break_label_depth = old_break;
        return;
    }

    case ND_CASE:
        gen_stmt(node->lhs);
        return;

    case ND_BLOCK:
        for (Node *n = node->body; n; n = n->next)
            gen_stmt(n);
        return;

    case ND_GOTO:
        // In WASM, break/continue are implemented via br instructions
        // chibicc generates ND_GOTO for break/continue statements
        if (break_label_depth >= 0) {
            // Calculate branch distance from current depth to break target
            int branch_depth = current_func->label_depth - break_label_depth;
            emit_br(current_func, branch_depth);
            return;
        }
        error_tok(node->tok, "goto not supported in WASM backend");
        return;

    case ND_LABEL:
        // Labels are handled with blocks in WASM
        gen_stmt(node->lhs);
        return;

    case ND_RETURN:
        if (node->lhs) {
            gen_expr(node->lhs);
            // Handle struct returns if needed
        }
        emit_return(current_func);
        return;

    case ND_EXPR_STMT:
        printf("ND_EXPR_STMT: lhs->kind=%d\n", node->lhs->kind);
        gen_expr(node->lhs);

        if (expr_has_value(node->lhs)) {
            printf("  -> emitting drop\n");
            emit_drop(current_func);
        }
        return;

    case ND_ASM:
        // Inline assembly not supported in WASM
        error_tok(node->tok, "inline assembly not supported in WASM backend");
        return;

    default:
        error_tok(node->tok, "invalid statement");
    }
}

// =============================================================================
// Local Variable Offset Assignment
// =============================================================================

static void assign_lvar_offsets(Obj *fn) {
    int offset = 0;

    // Parameters come first
    for (Obj *var = fn->params; var; var = var->next) {
        offset = (offset + var->align - 1) & ~(var->align - 1);
        var->offset = offset;
        offset += var->ty->size;
    }

    // Then local variables
    for (Obj *var = fn->locals; var; var = var->next) {
        offset = (offset + var->align - 1) & ~(var->align - 1);
        var->offset = offset;
        offset += var->ty->size;
    }

    fn->stack_size = (offset + 15) & ~15;  // 16-byte alignment
}

// =============================================================================
// Global Variable Emission
// =============================================================================

static void emit_data(Obj *prog) {
    printf("First pass...\n");
    for (Obj *var = prog; var; var = var->next) {
        if (var->is_function || !var->is_definition)
            continue;

        int offset;
        if (var->init_data) {
            offset = module_add_data(module, var->init_data, var->ty->size, var->align);
        } else {
            offset = module_reserve_bss(module, var->ty->size, var->align);
        }

        // Record the variable's memory offset
        symtab_add(module->global_syms, var->name, offset, -1, false);
    }
}

// =============================================================================
// Function Body Emission
// =============================================================================

static void emit_function(Obj *fn) {
    printf("emit_function: starting %s\n", fn->name);
    current_fn = fn;
    assign_lvar_offsets(fn);
    printf("emit_function: assigned lvar offsets\n");

    // DON'T register type/function here anymore - already done in pass 1
    // Just look up the function's param count for the builder
    int num_params = 0;
    for (Obj *p = fn->params; p; p = p->next)
        num_params++;

    // Build function body
    current_func = func_new(num_params);

    // Count locals (excluding params)
    int num_locals = 0;
    for (Obj *v = fn->locals; v; v = v->next)
        num_locals++;

    // For now, all locals are i32 (simplification)
    current_func->num_locals_i32 = num_locals + 1;  // +1 for frame pointer temp

    func_emit_locals(current_func);

    // Function prologue: allocate stack frame
    if (fn->stack_size > 0) {
        emit_global_get(current_func, module->stack_pointer_global);
        emit_i32_const(current_func, fn->stack_size);
        emit_op(current_func, OP_I32_SUB);
        emit_global_set(current_func, module->stack_pointer_global);
    }

    // Copy parameters to stack frame (if they need addresses taken)
    // For simplicity, always copy params to linear memory
    int param_idx = 0;
    for (Obj *p = fn->params; p; p = p->next, param_idx++) {
        emit_global_get(current_func, module->stack_pointer_global);
        emit_i32_const(current_func, p->offset);
        emit_op(current_func, OP_I32_ADD);
        emit_local_get(current_func, param_idx);
        store(p->ty);
    }

    // Generate body
    printf("emit_function: about to gen_stmt\n");
    if (fn->body)
        gen_stmt(fn->body);
    printf("emit_function: gen_stmt done\n");

    // Function epilogue: restore stack pointer
    if (fn->stack_size > 0) {
        emit_global_get(current_func, module->stack_pointer_global);
        emit_i32_const(current_func, fn->stack_size);
        emit_op(current_func, OP_I32_ADD);
        emit_global_set(current_func, module->stack_pointer_global);
    }

    // Implicit return for non-void functions (return 0 for main)
    if (fn->ty->return_ty->kind != TY_VOID) {
        if (strcmp(fn->name, "main") == 0)
            emit_i32_const(current_func, 0);
        else
            emit_i32_const(current_func, 0);  // Undefined behavior, but return something
    }

    emit_end(current_func);

    // Add function body to module
    buf_push_uleb128(module->code, current_func->code->len);
    buf_push_bytes(module->code, current_func->code->data, current_func->code->len);

    func_free(current_func);
    current_func = NULL;
}

// =============================================================================
// WASI Imports
// =============================================================================

static void add_wasi_imports(void) {
    // Add WASI fd_write for basic printf support
    // fd_write signature: (fd: i32, iovs: i32, iovs_len: i32, nwritten: i32) -> i32
    WasmType fd_write_params[] = {TYPE_I32, TYPE_I32, TYPE_I32, TYPE_I32};
    WasmType fd_write_result[] = {TYPE_I32};
    int type_idx = module_add_type(module, fd_write_params, 4, fd_write_result, 1);
    module_add_import_func(module, "wasi_snapshot_preview1", "fd_write", type_idx);

    // Add proc_exit
    WasmType proc_exit_params[] = {TYPE_I32};
    type_idx = module_add_type(module, proc_exit_params, 1, NULL, 0);
    module_add_import_func(module, "wasi_snapshot_preview1", "proc_exit", type_idx);
}

// =============================================================================
// Binary Output
// =============================================================================

static void write_section(Buffer *out, int section_id, Buffer *contents) {
    if (contents->len == 0)
        return;
    buf_push(out, section_id);
    buf_push_uleb128(out, contents->len);
    buf_push_bytes(out, contents->data, contents->len);
}

static void write_section_with_count(Buffer *out, int section_id, int count, Buffer *contents) {
    if (count == 0)
        return;

    Buffer *temp = buf_new();
    buf_push_uleb128(temp, count);
    buf_push_bytes(temp, contents->data, contents->len);

    buf_push(out, section_id);
    buf_push_uleb128(out, temp->len);
    buf_push_bytes(out, temp->data, temp->len);

    buf_free(temp);
}

static Buffer *module_finish(void) {
    Buffer *out = buf_new();

    // Magic number and version
    buf_push_bytes(out, "\0asm", 4);
    uint8_t version[] = {1, 0, 0, 0};
    buf_push_bytes(out, version, 4);

    // Type section
    write_section_with_count(out, SECTION_TYPE, module->num_types, module->types);

    // Import section
    write_section_with_count(out, SECTION_IMPORT, module->num_imports, module->imports);

    // Function section
    write_section_with_count(out, SECTION_FUNCTION, module->num_functions, module->functions);

    // Memory section
    Buffer *mem = buf_new();
    buf_push_uleb128(mem, 1);   // 1 memory
    buf_push(mem, 0x01);        // Has max
    buf_push_uleb128(mem, 2);   // Min 2 pages (128KB)
    buf_push_uleb128(mem, 256); // Max 256 pages (16MB)
    write_section(out, SECTION_MEMORY, mem);
    buf_free(mem);

    // Global section
    write_section_with_count(out, SECTION_GLOBAL, module->num_globals, module->globals);

    // Export section (also export memory)
    module_add_export(module, "memory", 2, 0);
    write_section_with_count(out, SECTION_EXPORT, module->num_exports, module->exports);

    // Code section
    write_section_with_count(out, SECTION_CODE, module->num_functions, module->code);

    // Data section
    write_section_with_count(out, SECTION_DATA, module->num_data_segments, module->data);

    return out;
}

// =============================================================================
// Main Entry Point
// =============================================================================

void codegen_wasm32(Obj *prog, FILE *out) {
    output_file = out;
    module = module_new();

    // Add WASI imports
    add_wasi_imports();

    // Add stack pointer global (starts at 64KB, grows down)
    module->stack_pointer_global = module_add_global_i32(module, "__stack_pointer", 65536, true);

    // First pass: emit global data
    emit_data(prog);

    // First pass: register all functions in symbol table
    for (Obj *fn = prog; fn; fn = fn->next) {
        if (!fn->is_function || !fn->is_definition)
            continue;
        if (fn->name[0] == '.')
            continue;

        // Count parameters and build type
        int num_params = 0;
        WasmType param_types[64];
        for (Obj *p = fn->params; p; p = p->next)
            param_types[num_params++] = get_wasm_type(p->ty);

        WasmType ret_types[1];
        int num_rets = 0;
        if (fn->ty->return_ty->kind != TY_VOID) {
            ret_types[0] = get_wasm_type(fn->ty->return_ty);
            num_rets = 1;
        }

        int type_idx = module_add_type(module, param_types, num_params, ret_types, num_rets);
        module_add_function(module, fn->name, type_idx);

        if (!fn->is_static)
            module_add_export(module, fn->name, 0, module->num_imports + module->num_functions - 1);
    }

    // Second pass: emit function bodies
    for (Obj *fn = prog; fn; fn = fn->next) {
        // ... existing emit_function code, but skip the type/function registration
    }

    // Second pass: emit functions
    printf("Second pass...\n");
    for (Obj *fn = prog; fn; fn = fn->next) {
        printf("Object: %s, is_function=%d, is_definition=%d, body=%p\n",
             fn->name, fn->is_function, fn->is_definition, (void*)fn->body);
        if (!fn->is_function || !fn->is_definition)
            continue;
        // (remove the is_live check entirely, or ensure parsing marks referenced functions)
        //if (!fn->is_live)
        //    continue;

        // Skip internal labels that aren't real functions
        if (fn->name[0] == '.')
            continue;

        // Skip functions without bodies
        if (!fn->body)
            continue;
        emit_function(fn);
    }

    // Generate WASM binary
    printf("Generating the wasm binary...\n");
    Buffer *wasm = module_finish();
    printf("Generated a wasm buffer of %lu bytes.\n", wasm->len);
    fwrite(wasm->data, 1, wasm->len, output_file);
    fflush(output_file);

    // Cleanup
    buf_free(wasm);
    // TODO: Free module and all buffers
}
