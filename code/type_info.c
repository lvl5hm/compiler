


#if 0


typedef enum {
  Type_Info_Kind_NONE,
  
  Type_Info_Kind_ARRAY,
  Type_Info_Kind_POINTER,
  Type_Info_Kind_STRUCT,
  Type_Info_Kind_INT,
  Type_Info_Kind_FLOAT,
  Type_Info_Kind_TYPE,
  Type_Info_Kind_ALIAS,
  Type_Info_Kind_FUNC,
} Type_Info_Kind;

typedef struct Type_Info Type_Info;

typedef struct {
  Type_Info *item_type;
  u64 count;
} Type_Info_Array;

typedef struct {
  Type_Info *base;
} Type_Info_Pointer;

typedef struct {
  String name;
  Type_Info *type;
} Type_Info_Struct_Member;

typedef struct {
  Type_Info_Struct_Member *members;
} Type_Info_Struct;

typedef struct {
  b32 __;
} Type_Info_Float;

typedef struct {
  b32 is_signed;
} Type_Info_Int;

typedef struct {
  b32 __;
} Type_Info_Type;

typedef struct {
  Type_Info **params;
  Type_Info *return_type;
} Type_Info_Func;

struct Type_Info {
  union {
    Type_Info_Array array;
    Type_Info_Pointer pointer;
    Type_Info_Float float_t;
    Type_Info_Int int_t;
    Type_Info_Type type;
    Type_Info_Func func;
    Type_Info_Struct struct_t;
  };
  Type_Info_Kind kind;
  i32 size;
};


Type_Info *builtin_Type = null;
Type_Info *builtin_f32 = null;
Type_Info *builtin_i32 = null;

Type_Info *code_type_to_type_info(Arena *arena, Code_Type *node) {
  Type_Info *info = arena_push_struct(arena, Type_Info);
  
  switch (node->kind) {
    case Type_Kind_FUNC: {
      info->kind = Type_Info_Kind_FUNC;
      info->func.params = sb_new(arena, Type_Info *, 8);
      for (u32 i = 0; i < sb_count(node->func.params); i++) {
        Type_Info *param_info = code_type_to_type_info(arena, node->func.params[i]->type);
        sb_push(info->func.params, param_info);
      }
      info->func.return_type = code_type_to_type_info(arena, node->func.return_type);
    } break;
    
    case Type_Kind_PTR: {
      info->kind = Type_Info_Kind_POINTER;
      info->pointer.base = code_type_to_type_info(arena, node->pointer.base);
    } break;
    
    case Type_Kind_STRUCT: {
      info->kind = Type_Info_Kind_STRUCT;
      info->struct_t.members = sb_new(arena, Type_Info_Struct_Member, 8);
      for (u32 i = 0; i < sb_count(node->struct_t.members); i++) {
        Code_Stmt_Decl *decl = node->struct_t.members[i];
        Type_Info_Struct_Member member;
        member.name = decl->name;
        member.type = code_type_to_type_info(arena, decl->type);
        
        sb_push(info->struct_t.members, member);
      }
    } break;
    
    case Type_Kind_ARRAY: {
      info->kind = Type_Info_Kind_STRUCT;
      info->array.count = node->array.count;
      info->array.item_type = code_type_to_type_info(arena, node->array.item_type);
    } break;
    
    case Type_Kind_ALIAS: {
      // TODO(lvl5): this needs to be a scope lookup
      if (string_compare(node->alias.name, const_string("i32"))) {
        info = builtin_i32;
      } else if (string_compare(node->alias.name, const_string("f32"))) {
        info = builtin_f32;
      } else if (string_compare(node->alias.name, const_string("Type"))) {
        info = builtin_Type;
      }
    } break;
    
    default: assert(false);
  }
  
  return info;
}
#endif
