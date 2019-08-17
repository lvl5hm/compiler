typedef enum {
  I_NONE,
  I_COMMENT,
  
  I_CONST,
  I_CONST_STACK,
  I_CONST_BSS,
  
  I_STACK_CHANGE,
  
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
  I_BIT_NOT,
  I_BIT_XOR,
  I_AND,
  I_OR,
  I_NOT,
  
  I_LOAD,
  
  I_STORE_8,
  I_STORE_16,
  I_STORE_32,
  I_STORE_64,
  
  I_GT_int,
  I_GT_f32,
  I_GT_f64,
  
  
  I_GTE_int,
  I_GTE_f32,
  I_GTE_f64,
  
  I_EQ,
  I_NE,
  
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
  I_RET,
  I_HLT,
} Instruction_Kind;

char *Instruction_Kind_To_String[] = {
  [I_NONE] = "NONE",
  
  [I_COMMENT] = "//",
  [I_CONST] = "CONST",
  [I_CONST_STACK] = "CONST_STACK",
  [I_CONST_BSS] = "CONST_BSS",
  [I_STACK_CHANGE] = "STACK_CHANGE",
  
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
  [I_BIT_NOT] = "BIT_NOT",
  [I_BIT_XOR] = "BIT_XOR",
  [I_NOT] = "NOT",
  [I_AND] = "AND",
  [I_OR] = "OR",
  
  [I_LOAD] = "LOAD",
  
  [I_STORE_8] = "STORE_8",
  [I_STORE_16] = "STORE_16",
  [I_STORE_32] = "STORE_32",
  [I_STORE_64] = "STORE_64",
  
  [I_GT_int] = "GT_int",
  [I_GT_f32] = "GT_f32",
  [I_GT_f64] = "GT_f64",
  
  [I_GTE_int] = "GTE_int",
  [I_GTE_f32] = "GTE_f32",
  [I_GTE_f64] = "GTE_f64",
  
  [I_EQ] = "EQ",
  [I_NE] = "NE",
  
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
  String *_comment;
  void *_ptr;
} Bc_Param;

typedef struct {
  Instruction_Kind kind;
  Bc_Param param;
} Bc_Instruction;

struct Bc_Emitter {
  Bc_Instruction *instructions;
  i64 instruction_count;
  
  byte *bss_segment;
  Code_Func *current_func;
  Parser *parser;
  i64 entry_instruction_index;
};

Bc_Instruction *bc_instruction(Bc_Emitter *e, Instruction_Kind kind, Bc_Param param) {
  Bc_Instruction instr = {0};
  instr.kind = kind;
  instr.param = param;
  e->instructions[e->instruction_count] = instr;
  return e->instructions + e->instruction_count++;
}


Bc_Param call_foreign(void *fn, Bc_Param *args, i32 count);

#define FOREIGN_FUNCTION_PTR_BIT (1LL << 63)
void bytecode_run(Arena *arena, Bc_Emitter *emitter) {
  byte *func_segment = (byte *)emitter->instructions;
  byte *bss_segment = emitter->bss_segment;
  byte *stack = arena_push_array(arena, byte, kilobytes(100));
  byte *heap = arena_push_array(arena, byte, megabytes(10));
  
  u32 stack_size = 0;
  
  Bc_Param exec[64];
  u32 exec_count = 0;
  
  b32 is_running = true;
  Bc_Instruction *instr = emitter->instructions + emitter->entry_instruction_index;
  
  while (is_running) {
    switch (instr->kind) {
      case I_COMMENT: {
        
      } break;
      
      case I_STACK_CHANGE: {
        Bc_Param param = instr->param;
        stack += param._i64;
      } break;
      
      case I_CALL: {
        Bc_Param func_param = exec[--exec_count];
        Bc_Param address = exec[--exec_count];
        
        exec[exec_count++] = instr->param;
        Code_Type_Func *func = (Code_Type_Func *)func_param._ptr;
        
        u64 is_foreign = address._u64 & FOREIGN_FUNCTION_PTR_BIT;
        if (is_foreign) {
          address._u64 &= ~FOREIGN_FUNCTION_PTR_BIT;
          i32 arg_count = sb_count(func->params);
          if (arg_count < 4) arg_count = 4;
          
          Bc_Param *args = sb_new(scratch_arena, Bc_Param, arg_count);
          
          Code_Type *return_type = get_final_type(func->return_type);
          i32 return_size = 0;
          if (return_type != builtin_void) {
            return_size = get_size_of_type(func->return_type);
          }
          i32 stack_offset = return_size; // for return value
          
          for (u32 arg_index = 0; arg_index < sb_count(func->params); arg_index++) {
            Code_Stmt_Decl *param = func->params[arg_index];
            i32 size = get_size_of_type(param->type);
            Bc_Param *arg = (Bc_Param *)(stack + stack_offset);
            stack_offset += size;
            
            if (size <= 8) {
              sb_push(args, *arg);
            } else {
              sb_push(args, (Bc_Param){._ptr = arg});
            }
          }
          
          // NOTE(lvl5): the call_foreign() function is kinda shitty,
          // the first 4 args in the array need to point to valid memory
          // and arg_count has to be >=4
          Bc_Param result = call_foreign(address._ptr, args, arg_count);
          
          if (return_type != builtin_void) {
            void *dst = stack + 0;
            switch (return_size) {
              case 1:
              *(i8 *)dst = result._i8;
              break;
              case 2:
              *(i16 *)dst = result._i16;
              break;
              case 4:
              *(i32 *)dst = result._i32;
              break;
              case 8:
              *(i64 *)dst = result._i64;
              break;
              
              default:
              copy_memory_slow(dst, result._ptr, return_size);
              break;
            }
            
          }
          
          // NOTE(lvl5): pop the return address off the exec, we don't need it
          exec_count--;
        } else {
          instr = (Bc_Instruction *)address._u64;
          continue;
        }
      } break;
      
      case I_RET: {
        Bc_Param address = exec[--exec_count];
        instr = (Bc_Instruction *)(emitter->instructions + address._u64);
        continue;
      } break;
      
      case I_CONST: {
        Bc_Param param = instr->param;
        exec[exec_count++] = param;
      } break;
      
      case I_CONST_STACK: {
        Bc_Param param = instr->param;
        param._u64 += (u64)stack;
        exec[exec_count++] = param;
      } break;
      
      case I_CONST_BSS: {
        Bc_Param param = instr->param;
        param._u64 += (u64)bss_segment;
        exec[exec_count++] = param;
      } break;
      
      case I_LOAD: {
        Bc_Param address = exec[--exec_count];
        u64 *item = address._ptr;
        exec[exec_count++] = (Bc_Param){ ._u64 = *item };
      } break;
      
      case I_MUL_int: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64*right._i64 };
      } break;
      
      case I_MUL_f32: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f32 = left._f32*right._f32 };
      } break;
      
      case I_MUL_f64: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f64 = left._f64*right._f64 };
      } break;
      
      case I_ADD_int: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 + right._i64 };
      } break;
      
      case I_ADD_f32: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f32 = left._f32 + right._f32 };
      } break;
      
      case I_ADD_f64: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f64 = left._f64 + right._f64 };
      } break;
      
      case I_SUB_int: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 + right._i64 };
      } break;
      
      case I_SUB_f32: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f32 = left._f32 + right._f32 };
      } break;
      
      case I_SUB_f64: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f64 = left._f64 + right._f64 };
      } break;
      
      case I_STORE_8: {
        Bc_Param address = exec[--exec_count];
        Bc_Param value = exec[--exec_count];
        i8 *loc = (i8 *)(address._u64);
        *loc = value._i8;
      } break;
      
      case I_STORE_16: {
        Bc_Param address = exec[--exec_count];
        Bc_Param value = exec[--exec_count];
        i16 *loc = (i16 *)(address._u64);
        *loc = value._i16;
      } break;
      
      case I_STORE_32: {
        Bc_Param address = exec[--exec_count];
        Bc_Param value = exec[--exec_count];
        i32 *loc = (i32 *)(address._u64);
        *loc = value._i32;
      } break;
      
      case I_STORE_64: {
        Bc_Param address = exec[--exec_count];
        Bc_Param value = exec[--exec_count];
        i64 *loc = (i64 *)(address._u64);
        *loc = value._i64;
      } break;
      
      case I_HLT: {
        is_running = false;
      } break;
      
      
      case I_DIV_int: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64/right._i64 };
      } break;
      
      case I_DIV_f32: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f32 = left._f32/right._f32 };
      } break;
      
      case I_DIV_f64: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f64 = left._f64/right._f64 };
      } break;
      
      case I_NEG_int: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = -right._i64 };
      } break;
      
      case I_NEG_f32: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f32 = -right._f32 };
      } break;
      
      case I_NEG_f64: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f64 = -right._f64 };
      } break;
      
      case I_MOD: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 % right._i64 };
      } break;
      
      case I_BIT_AND: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 & right._i64 };
      } break;
      
      case I_BIT_OR: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 | right._i64 };
      } break;
      
      case I_BIT_NOT: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = ~right._i64 };
      } break;
      
      case I_BIT_XOR: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 ^ right._i64 };
      } break;
      
      case I_AND: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 && right._i64 };
      } break;
      
      case I_OR: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 || right._i64 };
      } break;
      
      case I_NOT: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = !right._i64 };
      } break;
      
      case I_GT_int: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 > right._i64 };
      } break;
      
      case I_GT_f32: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._f32 > right._f32 };
      } break;
      
      case I_GT_f64: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._f64 > right._f64 };
      } break;
      
      
      case I_GTE_int: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 >= right._i64 };
      } break;
      
      case I_GTE_f32: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._f32 >= right._f32 };
      } break;
      
      case I_GTE_f64: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._f64 >= right._f64 };
      } break;
      
      case I_EQ: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 == right._i64 };
      } break;
      
      case I_NE: {
        Bc_Param right = exec[--exec_count];
        Bc_Param left = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = left._i64 != right._i64 };
      } break;
      
      case I_CAST_int_f32: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f32 = (f32)right._i64 };
      } break;
      
      case I_CAST_int_f64: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f64 = (f64)right._i64 };
      } break;
      
      case I_CAST_f32_int: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = (i64)right._f32 };
      } break;
      
      case I_CAST_f64_int: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._i64 = (i64)right._f64 };
      } break;
      
      case I_CAST_f64_f32: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f32 = (f32)right._f64 };
      } break;
      
      case I_CAST_f32_f64: {
        Bc_Param right = exec[--exec_count];
        exec[exec_count++] = (Bc_Param){ ._f64 = (f64)right._f32 };
      } break;
      
      case I_JMP_TRUE: {
        Bc_Param right = exec[--exec_count];
        if (right._i64) {
          instr += right._i64;
          continue;
        }
      } break;
      
      case I_JMP_FALSE: {
        Bc_Param right = exec[--exec_count];
        Bc_Param offset = instr->param;
        if (!right._i64) {
          instr += offset._i64;
          continue;
        }
      } break;
      
      case I_JMP: {
        Bc_Param offset = instr->param;
        instr += offset._i64;
        continue;
      } break;
      
      case I_MEMCOPY: {
        Bc_Param to = exec[--exec_count];
        Bc_Param from = exec[--exec_count];
        Bc_Param size = instr->param;
        copy_memory_slow((byte *)to._u64, (byte *)from._u64, size._u64);
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
    printf("%04d  %s", i, mnemonic);
    
    switch (instr.kind) {
      case I_COMMENT:
      printf(" %s", tcstring(*instr.param._comment));
      break;
      
      case I_CONST:
      case I_CONST_STACK:
      case I_CONST_BSS:
      case I_CALL:
      case I_JMP_TRUE:
      case I_JMP_FALSE:
      case I_JMP:
      case I_STACK_CHANGE:
      case I_MEMCOPY:
      printf(" %lld", instr.param._i64);
      break;
      
      case I_SUB_f64:
      case I_ADD_f64:
      case I_MUL_f64:
      case I_DIV_f64:
      case I_NEG_f64:
      case I_GT_f64:
      case I_GTE_f64:
      case I_ADD_f32:
      case I_SUB_f32:
      case I_MUL_f32:
      case I_DIV_f32:
      case I_NEG_f32:
      case I_GT_f32:
      case I_GTE_f32:
      case I_CAST_f32_f64:
      case I_CAST_f64_f32:
      case I_CAST_f64_int:
      case I_CAST_f32_int:
      case I_HLT:
      case I_ADD_int:
      case I_SUB_int:
      case I_MUL_int:
      case I_DIV_int:
      case I_NEG_int:
      case I_MOD:
      case I_BIT_AND:
      case I_BIT_OR:
      case I_BIT_NOT:
      case I_BIT_XOR:
      case I_AND:
      case I_OR:
      case I_NOT:
      case I_LOAD:
      case I_STORE_8:
      case I_STORE_16:
      case I_STORE_32:
      case I_STORE_64:
      case I_GT_int:
      case I_GTE_int:
      case I_EQ:
      case I_NE:
      case I_CAST_int_f32:
      case I_CAST_int_f64:
      break;
      
      case I_RET:
      printf("\n");
      break;
      
      default: assert(false);
      break;
    }
    
    printf("\n");
  }
}

#define PARAM(T, val) (Bc_Param){ ._##T = val }

void bc_emit_expr(Bc_Emitter *, Code_Expr *);

void bc_store_size(Bc_Emitter *e, i32 size) {
  switch (size) {
    case 1:
    bc_instruction(e, I_STORE_8, NULL_PARAM);
    break;
    
    case 2:
    bc_instruction(e, I_STORE_16, NULL_PARAM);
    break;
    
    case 4:
    bc_instruction(e, I_STORE_32, NULL_PARAM);
    break;
    
    case 8:
    bc_instruction(e, I_STORE_64, NULL_PARAM);
    break;
    
    default:
    bc_instruction(e, I_MEMCOPY, PARAM(i64, size));
    break;
  }
};


void bc_emit_func_call(Bc_Emitter *e, Code_Expr_Call *call) {
  Code_Type_Func *func = &call->func->type->func;
  
  i32 total_args_size = 0;
  b32 is_void = get_final_type(func->return_type) != builtin_void;
  if (is_void) {
    total_args_size += get_size_of_type(func->return_type);
  }
  
  for (u32 i = 0; i < sb_count(call->args); i++) {
    Code_Expr *arg = call->args[i];
    i32 size = get_size_of_type(arg->type);
    
    bc_emit_expr(e, arg);
    if (size > 8) {
      assert(e->instructions[e->instruction_count-1].kind == I_LOAD);
      e->instruction_count--;
    }
    bc_instruction(e, I_CONST_STACK, PARAM(i64, e->current_func->stack_size + total_args_size));
    bc_store_size(e, size);
    
    total_args_size += size;
  }
  
  bc_emit_expr(e, call->func);
  bc_instruction(e, I_STACK_CHANGE, PARAM(i64, e->current_func->stack_size));
  
  
  bc_instruction(e, I_CONST, PARAM(ptr, func));
  bc_instruction(e, I_CALL, PARAM(i64, e->instruction_count+1));
  
  bc_instruction(e, I_STACK_CHANGE, PARAM(i64, -e->current_func->stack_size));
  
  if (is_void) {
    // load the return value into the exec stack
    bc_instruction(e, I_CONST_STACK, PARAM(i64, e->current_func->stack_size));
    bc_instruction(e, I_LOAD, NULL_PARAM);
  }
}

void bc_emit_expr(Bc_Emitter *e, Code_Expr *expr) {
  switch (expr->kind) {
    case Expr_Kind_TYPE: {
      assert(false);
    } break;
    case Expr_Kind_NAME: {
      Code_Stmt_Decl *decl = expr->name.decl;
      if (decl->storage_kind == Storage_Kind_BSS) {
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
            instr = e->instructions + --e->instruction_count - 1;
          }
          assert(instr->kind == I_CONST_STACK || 
                 instr->kind == I_CONST_BSS);
          instr->param._i64 += name_decl->offset;
          bc_instruction(e, I_LOAD, NULL_PARAM);
        } break;
        case T_SUBSCRIPT: {
          Code_Type *right_type = get_final_type(expr->binary.right->type);
          assert(right_type->kind == Type_Kind_INT);
          bc_emit_expr(e, expr->binary.left);
          bc_emit_expr(e, expr->binary.right);
          bc_instruction(e, I_ADD_int, NULL_PARAM);
          bc_instruction(e, I_LOAD, NULL_PARAM);
        } break;
        
        case T_ADD: {
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
        
        case T_GREATER: {
          bc_emit_expr(e, expr->binary.left);
          bc_emit_expr(e, expr->binary.right);
          bc_instruction(e, I_GT_int, NULL_PARAM);
        } break;
        
        case T_LESS: {
          bc_emit_expr(e, expr->binary.right);
          bc_emit_expr(e, expr->binary.left);
          bc_instruction(e, I_GT_int, NULL_PARAM);
        } break;
        
        default: {
          assert(false);
        } break;
      }
    } break;
    case Expr_Kind_CALL: {
      bc_emit_func_call(e, &expr->call);
    } break;
    case Expr_Kind_CAST: {
      if (expr->cast.implicit) {
        bc_emit_expr(e, expr->cast.expr);
      } else {
        // TODO(lvl5): actual casts
        bc_emit_expr(e, expr->cast.expr);
      }
    } break;
    case Expr_Kind_CHAR: {
      bc_instruction(e, I_CONST, PARAM(i64, expr->char_e.value));
    } break;
    case Expr_Kind_INT: {
      Placeholder *pl = expr->int_e.placeholder;
      if (pl) {
        assert(pl->storage_kind == Storage_Kind_BSS);
        byte *dst = e->bss_segment + pl->offset;
        copy_memory_slow(dst, pl->data, pl->size);
        expr->int_e.value = (u64)dst;
      }
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


void bc_emit_stmt_block(Bc_Emitter *, Code_Stmt_Block *);

void bc_emit_stmt(Bc_Emitter *e, Code_Stmt *stmt) {
  {
    String *comment = arena_push_struct(scratch_arena, String);
    Code_Node *node = (Code_Node *)stmt;
    Token t = e->parser->tokens[node->first_token];
    *comment = get_line_from_index(e->parser->src, (u32)(t.value.data - e->parser->src.data));
    
    bc_instruction(e, I_COMMENT, PARAM(comment, comment));
  }
  
  switch (stmt->kind) {
    case Stmt_Kind_ASSIGN: {
      // TODO(lvl5):  if assigning struct, right should also be no deref
      bc_emit_expr(e, stmt->assign.right);
      bc_emit_expr(e, stmt->assign.left);
      assert(e->instructions[e->instruction_count-1].kind == I_LOAD);
      e->instruction_count--;
      i32 size = get_size_of_type(stmt->assign.left->type);
      bc_store_size(e, size);
    } break;
    case Stmt_Kind_EXPR: {
      bc_emit_expr(e, stmt->expr.expr);
    } break;
    case Stmt_Kind_IF: {
      bc_emit_expr(e, stmt->if_s.cond);
      Bc_Instruction *then_jmp = bc_instruction(e, I_JMP_FALSE, NULL_PARAM);
      i64 then_count = e->instruction_count;
      
      bc_emit_stmt(e, stmt->if_s.then_branch);
      if (stmt->if_s.else_branch) {
        Bc_Instruction *else_jmp = bc_instruction(e, I_JMP, NULL_PARAM);
        i64 else_count = e->instruction_count;
        
        then_jmp->param._i64 = e->instruction_count - then_count+1;
        bc_emit_stmt(e, stmt->if_s.else_branch);
        else_jmp->param._i64 = e->instruction_count - else_count+1;
      } else {
        then_jmp->param._i64 = e->instruction_count - then_count+1;
      }
    } break;
    case Stmt_Kind_BLOCK: {
      bc_emit_stmt_block(e, &stmt->block);
    } break;
    case Stmt_Kind_WHILE: {
      i64 begin_pos = e->instruction_count;
      bc_emit_expr(e, stmt->while_s.cond);
      
      i64 jmp_to_end_index = e->instruction_count;
      Bc_Instruction *jmp_to_end = bc_instruction(e, I_JMP_FALSE, NULL_PARAM);
      bc_emit_stmt(e, stmt->while_s.body);
      bc_instruction(e, I_JMP, PARAM(i64, begin_pos - e->instruction_count));
      jmp_to_end->param._i64 = e->instruction_count - jmp_to_end_index;
    } break;
    case Stmt_Kind_FOR: {
      assert(false);
    } break;
    case Stmt_Kind_KEYWORD: {
      switch (stmt->keyword.keyword) {
        case T_RETURN: {
          // TODO(lvl5): if is entry, I_HLT instead
          if (stmt->keyword.stmt) {
            assert(stmt->keyword.stmt->kind == Stmt_Kind_EXPR);
            Code_Expr *expr = stmt->keyword.stmt->expr.expr;
            bc_emit_expr(e, expr);
            i32 size = get_size_of_type(expr->type);
            if (size > 8) {
              //assert(get_final_type(expr->type)-> == Type_Kind_STRUCT);
              assert(e->instructions[e->instruction_count-1].kind == I_LOAD);
              e->instruction_count--;
            }
            bc_instruction(e, I_CONST_STACK, NULL_PARAM);
            bc_store_size(e, size);
          }
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
      bc_emit_decl(e, &stmt->decl, Resolve_State_FULL);
    } break;
    default: assert(false);
  }
}

void bc_emit_stmt_block(Bc_Emitter *e, Code_Stmt_Block *block) {
  for (u32 i = 0; i < sb_count(block->statements); i++) {
    bc_emit_stmt(e, block->statements[i]);
  }
}

extern void *LoadLibraryA(char *file_name);
extern void *GetProcAddress(void *library, char *proc_name);

void bc_emit_decl(Bc_Emitter *e, Code_Stmt_Decl *decl, Resolve_State state) {
  if (decl->emit_state == Resolve_State_FULL) {
    return;
  }
  decl->emit_state = Resolve_State_FULL;
  
  if (!decl->value) {
    return;
  }
  switch (decl->value->kind) {
    case Code_Kind_FUNC: {
      String *comment = arena_push_struct(scratch_arena, String);
      *comment = concat(scratch_arena, const_string("func "), decl->name);
      
      bc_instruction(e, I_COMMENT, PARAM(comment, comment));
      
      
      // NOTE(lvl5): handle main() entry point
      b32 is_entry = string_compare(decl->name, const_string("__main"));
      if (is_entry) {
        e->entry_instruction_index = e->instruction_count;
      }
      
      Code_Func *old_func = e->current_func;
      e->current_func = &decl->value->func;
      
      if (e->current_func->foreign) {
        char *library_name = tcstring(e->current_func->module);
        void *library = LoadLibraryA(library_name);
        u64 proc = (u64)GetProcAddress(library, tcstring(e->current_func->foreign_name));
        proc |= FOREIGN_FUNCTION_PTR_BIT;
        *(u64 *)(e->bss_segment + decl->offset) = proc;
      } else {
        *(u64 *)(e->bss_segment + decl->offset) = (u64)(e->instructions + e->instruction_count);
        bc_emit_stmt_block(e, decl->value->func.body);
      }
      
      e->current_func = &decl->value->func;
      
      if (e->instructions[e->instruction_count-1].kind != I_RET) {
        bc_instruction(e, is_entry ? I_HLT : I_RET, NULL_PARAM);
      }
    } break;
    
    case Code_Kind_EXPR: {
      if (decl->value->expr.kind != Expr_Kind_TYPE) {
        i32 size = get_size_of_type(decl->type);
        bc_emit_expr(e, &decl->value->expr);
        if (size > 8) {
          assert(e->instructions[e->instruction_count-1].kind == I_LOAD);
          e->instruction_count--;
        }
        
        if (decl->storage_kind == Storage_Kind_BSS) {
          // TODO(lvl5): this is wrong. we do have constant expressions
          // on the stack and mutable in the BSS
          bc_instruction(e, I_CONST_BSS, PARAM(i64, decl->offset));
        } else if (decl->storage_kind == Storage_Kind_STACK) {
          bc_instruction(e, I_CONST_STACK, PARAM(i64, decl->offset));
        }
        
        bc_store_size(e, size);
      }
    } break;
    
    default: assert(false);
  }
}
