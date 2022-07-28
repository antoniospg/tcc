.globl main
main:
	movq $1, %rax
	push %rax
	movq $1, %rax
	pop %rcx
	addq %rcx, %rax
	push %rax
	movq $4, %rax
	pop %rcx
	addq %rcx, %rax
	ret
