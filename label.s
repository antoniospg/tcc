.globl main 

_e3:
  addq $1, %rax

main: 
  movq $0, %rax
  cmpq $0, %rax
  je _e3

  addq $1, %rax

  jmp _post_cond

  _e3:
    addq $1, %rax
  
  _post_cond:
    addq $2, %rax
    ret

