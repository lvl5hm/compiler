typedef enum {
  I_NONE,
  
  I_CONST,
  I_CONST_STACK,
  I_CONST_BSS,
  
  I_ADD_int,
  I_ADD_f32,
  I_ADD_f64,
  
  I_SUB_int,
  I_SUB_f32,
  I_SUB_f64,
  
  I_MUL_int,
  I_MUL_f32,
  I_MUL_f64,
  
  I_DIV_int,
  I_DIV_f32,
  I_DIV_f64,
  
  I_NEG_int,
  I_NEG_f32,
  I_NEG_f64,
  
  I_MOD,
  I_BIT_AND,
  I_BIT_OR,
  I_BIT_NEG,
  I_BIT_XOR,
  
  I_LOAD,
  
  I_STORE_i8,
  I_STORE_i16,
  I_STORE_i32,
  I_STORE_i64,
  I_STORE_f32,
  I_STORE_f64,
  
  I_GT_int,
  I_GT_f32,
  I_GT_f64,
  
  I_GTE_int,
  I_GTE_f32,
  I_GTE_f64,
  
  I_EQ_int,
  I_EQ_f32,
  I_EQ_f64,
  
  I_NE_int,
  I_NE_f32,
  I_NE_f64,
  
  I_CAST_int_f32,
  I_CAST_int_f64,
  I_CAST_f32_int,
  I_CAST_f64_int,
  I_CAST_f64_f32,
  I_CAST_f32_f64,
  
  I_JMP_TRUE,
  I_JMP_FALSE,
  I_JMP,
  
  I_MEMCOPY,
  I_CALL,
  I_CALL_FOREIGN,
  I_RET,
  I_HLT,
} Instruction_Kind;

char *Instruction_Kind_To_String[] = {
  [I_NONE] = "NONE",
  
  [I_CONST] = "CONST",
  [I_CONST_STACK] = "CONST_STACK",
  [I_CONST_BSS] = "CONST_BSS",
  
  [I_ADD_int] = "ADD_int",
  [I_ADD_f32] = "ADD_f32",
  [I_ADD_f64] = "ADD_f64",
  
  [I_SUB_int] = "SUB_int",
  [I_SUB_f32] = "SUB_f32",
  [I_SUB_f64] = "SUB_f64",
  
  [I_MUL_int] = "MUL_int",
  [I_MUL_f32] = "MUL_f32",
  [I_MUL_f64] = "MUL_f64",
  
  [I_DIV_int] = "DIV_int",
  [I_DIV_f32] = "DIV_f32",
  [I_DIV_f64] = "DIV_f64",
  
  [I_NEG_int] = "NEG_int",
  [I_NEG_f32] = "NEG_f32",
  [I_NEG_f64] = "NEG_f64",
  
  [I_MOD] = "MOD",
  [I_BIT_AND] = "BIT_AND",
  [I_BIT_OR] = "BIT_OR",
  [I_BIT_NEG] = "BIT_NEG",
  [I_BIT_XOR] = "BIT_XOR",
  
  [I_LOAD] = "LOAD",
  
  [I_STORE_i8] = "STORE_i8",
  [I_STORE_i16] = "STORE_i16",
  [I_STORE_i32] = "STORE_i32",
  [I_STORE_i64] = "STORE_i64",
  [I_STORE_f32] = "STORE_f32",
  [I_STORE_f64] = "STORE_f64",
  
  [I_GT_int] = "GT_int",
  [I_GT_f32] = "GT_f32",
  [I_GT_f64] = "GT_f64",
  
  [I_GTE_int] = "GTE_int",
  [I_GTE_f32] = "GTE_f32",
  [I_GTE_f64] = "GTE_f64",
  
  [I_EQ_int] = "EQ_int",
  [I_EQ_f32] = "EQ_f32",
  [I_EQ_f64] = "EQ_f64",
  
  [I_NE_int] = "NE_int",
  [I_NE_f32] = "NE_f32",
  [I_NE_f64] = "NE_f64",
  
  [I_CAST_int_f32] = "CAST_int_f32",
  [I_CAST_int_f64] = "CAST_int_f64",
  [I_CAST_f32_int] = "CAST_f32_int",
  [I_CAST_f64_int] = "CAST_f64_int",
  [I_CAST_f64_f32] = "CAST_f64_f32",
  [I_CAST_f32_f64] = "CAST_f32_f64",
  
  [I_JMP_TRUE] = "JMP_TRUE",
  [I_JMP_FALSE] = "JMP_FALSE",
  [I_JMP] = "JMP",
  
  [I_MEMCOPY] = "MEMCOPY",
  [I_CALL] = "CALL",
  [I_CALL_FOREIGN] = "CALL_FOREIGN",
  [I_RET] = "RET",
  [I_HLT] = "HLT",
};

typedef union {
  i8 _i8;
  i16 _i16;
  i32 _i32;
  i64 _i64;
  f32 _f32;
  f64 _f64;
  u64 _u64;
  u32 _u32;
  u16 _u16;
  u8 _u8;
} Bc_Param;

typedef struct {
  u8 kind;
  Bc_Param param;
} Bc_Instruction;

struct Bc_Emitter {
  Bc_Instruction *instructions;
  u32 instruction_count;
  u32 bss_size;
};

void bc_instruction(Bc_Emitter *e, Instruction_Kind kind, Bc_Param param) {
  Bc_Instruction instr = {0};
  instr.kind = kind;
  instr.param = param;
  e->instructions[e->instruction_count++] = instr;
}

typedef struct {
  byte *memory;
  u64 bss_begin;
  u64 stack_begin;
  u64 heap_begin;
} Bytecode_Runner;



void bytecode_run(Arena *arena, Bc_Emitter *emitter) {
  Bytecode_Runner vm = {0};
  vm.memory = (byte *)emitter->instructions;
  vm.bss_begin = emitter->instruction_count*sizeof(Bc_Instruction);
  vm.stack_begin = vm.bss_begin + kilobytes(5);
  vm.heap_begin = vm.stack_begin + kilobytes(1024);
  
  u32 stack_size = 0;
  
  Bc_Param exec[64];
  u32 exec_count = 0;
  
  b32 is_running = true;
  Bc_Instruction *instr = emitter->instructions;
  
  while (is_running) {
    switch (instr->kind) {
      case I_CONST: {
        Bc_Param param = instr->param;
        exec[exec_count++] = param;
      } break;
      
      case I_CONST_STACK: {
        Bc_Param param = instr->param;
        param._i64 += vm.stack_begin + stack_size;
        exec[exec_count++] = param;
      } break;
      
      case I_LOAD: {
        Bc_Param address = exec[--exec_count];
        u64 *item = (u64 *)(vm.memory + address._u64);
        exec[exec_count++] = (Bc_Param){ ._u64 = *item };
      } break;
      
      case I_MUL_int: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64*right._i64 };
      } break;
      
      case I_ADD_int: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 + right._i64 };
      } break;
      
      case I_STORE_i32: {
        Bc_Param address = exec[--exec_count];
        Bc_Param value = exec[--exec_count];
        i32 *loc = (i32 *)(vm.memory + address._u64);
        *loc = value._i32;
      } break;
      
      case I_HLT: {
        is_running = false;
      } break;
      
      default: assert(false);
    }
    instr++;
  }
}

#define NULL_PARAM (Bc_Param){0}



void bytecode_print(Bc_Emitter *e) {
  for (u32 i = 0; i < e->instruction_count; i++) {
    Bc_Instruction instr = e->instructions[i];
    char *mnemonic = Instruction_Kind_To_String[instr.kind];
    printf("%s", mnemonic);
    if (instr.param._u64 != NULL_PARAM._u64) {
      printf(" %lld", instr.param._i64);
    }
    printf("\n");
  }
}

#define PARAM(T, val) (Bc_Param){ ._##T = val }
void bytecode_test(Arena *arena) {
  Bc_Emitter emitter = {0};
  emitter.instructions = arena_push_array(arena, Bc_Instruction, 2048);
  Bc_Emitter *e = &emitter;
  
  
  
  bc_instruction(e, I_CONST, PARAM(i32, 55));
  bc_instruction(e, I_CONST_STACK, PARAM(i64, 0));
  bc_instruction(e, I_STORE_i32, NULL_PARAM);
  
  bc_instruction(e, I_CONST, PARAM(i32, 10));
  bc_instruction(e, I_CONST, PARAM(i32, 2));
  bc_instruction(e, I_CONST_STACK, PARAM(i32, 0));
  bc_instruction(e, I_LOAD, NULL_PARAM);
  bc_instruction(e, I_MUL_int, NULL_PARAM);
  bc_instruction(e, I_ADD_int, NULL_PARAM);
  bc_instruction(e, I_CONST_STACK, PARAM(i32, 8));
  bc_instruction(e, I_STORE_i32, NULL_PARAM);
  bc_instruction(e, I_HLT, NULL_PARAM);
  
  //bytecode_run(arena, &emitter);
}


void bc_emit_expr(Bc_Emitter *e, Code_Expr *expr) {
  switch (expr->kind) {
    case Expr_Kind_TYPE: {
      assert(false);
    } break;
    case Expr_Kind_NAME: {
      Code_Stmt_Decl *decl = expr->name.decl;
      if (decl->is_global) {
        bc_instruction(e, I_CONST_BSS, PARAM(i32, decl->offset));
      } else {
        bc_instruction(e, I_CONST_STACK, PARAM(i32, decl->offset));
      }
      bc_instruction(e, I_LOAD, NULL_PARAM); // TODO(lvl5): types
    } break;
    case Expr_Kind_UNARY: {
      Token_Kind op = expr->unary.op;
      switch (op) {
        case T_REF: {
          // TODO(lvl5): should only work on variables
          bc_emit_expr(e, expr->unary.val);
          assert(e->instructions[e->instruction_count-1].kind == I_LOAD);
          e->instruction_count--;
        } break;
        
        case T_DEREF: {
          // TODO(lvl5): make sure assigning into dereferenced variable works
          bc_instruction(e, I_LOAD, NULL_PARAM);
        } break;
        
        default: assert(false);
      }
    } break;
    case Expr_Kind_BINARY: {
      switch (expr->binary.op) {
        case T_MEMBER: {
          bc_emit_expr(e, expr->binary.left);
          assert(expr->binary.right->kind == Expr_Kind_NAME);
          
          // TODO(lvl5): name.decl should map to a struct member decl here
          Code_Stmt_Decl *name_decl = expr->binary.right->name.decl;
          Bc_Instruction *instr = e->instructions + e->instruction_count-1;
          if (expr->binary.left->type->kind != Type_Kind_PTR) {
            assert(instr->kind == I_LOAD);
            instr = e->instructions + --e->instruction_count;
          }
          assert(instr->kind == I_CONST_STACK || 
                 instr->kind == I_CONST_BSS);
          instr->param._i64 += name_decl->offset;
          bc_instruction(e, I_LOAD, NULL_PARAM);
        } break;
        case T_SUBSCRIPT: {
          assert(false);
        } break;
        
        case T_ADD: {
          // TODO(lvl5): we can probably have one instruction for ints and 2 for floats
          bc_emit_expr(e, expr->binary.left);
          bc_emit_expr(e, expr->binary.right);
          bc_instruction(e, I_ADD_int, NULL_PARAM);
        } break;
        
        case T_SUB: {
          bc_emit_expr(e, expr->binary.left);
          bc_emit_expr(e, expr->binary.right);
          bc_instruction(e, I_SUB_int, NULL_PARAM);
        } break;
        
        case T_MUL: {
          bc_emit_expr(e, expr->binary.left);
          bc_emit_expr(e, expr->binary.right);
          bc_instruction(e, I_MUL_int, NULL_PARAM);
        } break;
        
        default: {
          assert(false);
        } break;
      }
    } break;
    case Expr_Kind_CALL: {
      assert(false);
      bc_emit_expr(e, expr->call.func);
#if 0
      assert(e->instructions[e->instruction_count-1].kind == I_LOAD);
      e->instruction_count--;
#endif
      
      u32 total_args_size = 0;
      for (u32 i = 0; i < sb_count(expr->call.args); i++) {
        Code_Expr *arg = expr->call.args[i];
        bc_emit_expr(e, arg);
        assert(e->instructions[e->instruction_count-1].kind == I_LOAD);
        e->instruction_count--;
        // TODO(lvl5): store the argument on the stack
        total_args_size += get_size_of_type(arg->type);
      }
      bc_instruction(e, I_CALL, PARAM(i32, total_args_size));
    } break;
    case Expr_Kind_CAST: {
      if (expr->cast.implicit) {
        bc_emit_expr(e, expr->cast.expr);
      } else {
        assert(false);
      }
    } break;
    case Expr_Kind_INT: {
      bc_instruction(e, I_CONST, PARAM(i64, expr->int_e.value));
    } break;
    case Expr_Kind_FLOAT: {
      bc_instruction(e, I_CONST, PARAM(f64, expr->float_e.value));
    } break;
    case Expr_Kind_STRING: {
      assert(false);
    } break;
    case Expr_Kind_NULL: {
      bc_instruction(e, I_CONST, PARAM(i64, 0));
    } break;
    
    default: assert(false);
  }
}

void bc_emit_decl(Bc_Emitter *e, Code_Stmt_Decl *decl);

void bc_emit_stmt_block(Bc_Emitter *, Code_Stmt_Block *);

void bc_emit_stmt(Bc_Emitter *e, Code_Stmt *stmt) {
  switch (stmt->kind) {
    case Stmt_Kind_ASSIGN: {
      // TODO(lvl5):  if struct, right should also be no deref
      bc_emit_expr(e, stmt->assign.right);
      bc_emit_expr(e, stmt->assign.left);
      assert(e->instructions[e->instruction_count-1].kind == I_LOAD);
      e->instruction_count--;
      
      switch (stmt->assign.op) {
        case T_ASSIGN: {
          if (stmt->assign.left->type == builtin_i32) {
            bc_instruction(e, I_STORE_i32, NULL_PARAM);
          } else {
            assert(false);
          }
        } break;
        
        default: assert(false);
      }
    } break;
    case Stmt_Kind_EXPR: {
      bc_emit_expr(e, stmt->expr.expr);
    } break;
    case Stmt_Kind_IF: {
      assert(false);
    } break;
    case Stmt_Kind_BLOCK: {
      bc_emit_stmt_block(e, &stmt->block);
      assert(false);
    } break;
    case Stmt_Kind_FOR: {
      assert(false);
    } break;
    case Stmt_Kind_KEYWORD: {
      switch (stmt->keyword.keyword) {
        case T_RETURN: {
          bc_instruction(e, I_RET, NULL_PARAM);
        } break;
        
        case T_PUSH_CONTEXT: {
          bc_emit_stmt(e, stmt->keyword.stmt);
        } break;
        
        case T_DEFER: break;
        
        default: assert(false);
      }
    } break;
    case Stmt_Kind_DECL: {
      bc_emit_decl(e, &stmt->decl);
    } break;
    default: assert(false);
  }
}

void bc_emit_stmt_block(Bc_Emitter *e, Code_Stmt_Block *block) {
  for (u32 i = 0; i < sb_count(block->statements); i++) {
    bc_emit_stmt(e, block->statements[i]);
  }
}

void bc_emit_decl(Bc_Emitter *e, Code_Stmt_Decl *decl) {
  switch (decl->value->kind) {
    case Code_Kind_FUNC: {
      bc_emit_stmt_block(e, decl->value->func.body);
      // TODO(lvl5): assign func in the bss
    } break;
    
    case Code_Kind_EXPR: {
      bc_emit_expr(e, &decl->value->expr);
      if (decl->is_const) {
        bc_instruction(e, I_CONST_BSS, PARAM(i64, decl->offset));
      } else {
        bc_instruction(e, I_CONST_STACK, PARAM(i64, decl->offset));
      }
      bc_instruction(e, I_STORE_i32, NULL_PARAM);
    } break;
    
    default: assert(false);
  }
}
