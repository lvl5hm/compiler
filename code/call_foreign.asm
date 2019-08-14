_DATA SEGMENT

_DATA ENDS

_TEXT SEGMENT

EXTERN putchar: PROC
PUBLIC call_foreign


call_foreign PROC
push rbp
mov rbp, rsp

mov r10, rcx ; func
mov r11, rdx ; args
mov r12, r8 ; count
sub r12, 4



mov rcx, [r11]
movsd XMM0, qword ptr [r11]
mov rdx, [r11 + 8]
movsd XMM1, qword ptr [r11 + 8]
mov r8, [r11 + 16]
movsd XMM2, qword ptr [r11 + 16]
mov r9, [r11 + 24]
movsd XMM3, qword ptr [r11 + 24]

cmp r12, 0
jz loop_end

mov rax, r12
mov r13, 8
mul r13
add rax, 24

loop_begin:
push [r11 + rax]

sub rax, 8

dec r12
cmp r12, 0
jnz loop_begin
loop_end:

sub rsp, 8*4
call r10

mov rsp, rbp
pop rbp

ret
call_foreign ENDP



_TEXT ENDS

END
