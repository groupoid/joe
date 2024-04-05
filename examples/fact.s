.data
.balign	8
.text
fact.6:
	cmpq	$1, %rax
	jg	jle_else.17
	movq	$1, %rax
	ret
jle_else.17:
	movq	%rax, %rbx
	subq	$1, %rbx
	movq	%rax, 0(%rbp)
	movq	%rbx, %rax
	addq	$8, %rbp
	call	fact.6
	subq	$8, %rbp
	movq	0(%rbp), %rbx
	mul	%rbx, %rax
	ret
.globl	min_caml_start
min_caml_start:
.globl	_min_caml_start
_min_caml_start:
	pushq	%rax
	pushq	%rbx
	pushq	%rcx
	pushq	%rdx
	pushq	%rsi
	pushq	%rdi
	pushq	%rbp
	movq	%rdi,%rbp
	movq	%rsi,%rax
	movq	%rax,min_caml_hp
	call	min_caml_read_int
	call	fact.6
	call	min_caml_print_int
	popq	%rbp
	popq	%rdi
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%rbx
	popq	%rax
	ret
