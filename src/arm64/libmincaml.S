	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 12, 0	sdk_version 13, 1
	.globl	min_caml_print_int              ; -- Begin function min_caml_print_int
	.p2align	2
min_caml_print_int:                     ; @"\01min_caml_print_int"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	str	x0, [sp, #8]
	ldr	x8, [sp, #8]
	mov	x9, sp
	str	x8, [x9]
	adrp	x0, l_.str@PAGE
	add	x0, x0, l_.str@PAGEOFF
	bl	_printf
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_print_newline          ; -- Begin function min_caml_print_newline
	.p2align	2
min_caml_print_newline:                 ; @"\01min_caml_print_newline"
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	adrp	x0, l_.str.1@PAGE
	add	x0, x0, l_.str.1@PAGEOFF
	bl	_printf
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_create_array           ; -- Begin function min_caml_create_array
	.p2align	2
min_caml_create_array:                  ; @"\01min_caml_create_array"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #48
	.cfi_def_cfa_offset 48
	str	x0, [sp, #40]
	str	x1, [sp, #32]
	; InlineAsm Start
	mov	x8, x27
	; InlineAsm End
	str	x8, [sp, #24]
	ldr	x8, [sp, #24]
	str	x8, [sp, #16]
	str	xzr, [sp, #8]
	b	LBB2_1
LBB2_1:                                 ; =>This Inner Loop Header: Depth=1
	ldr	x8, [sp, #8]
	ldr	x9, [sp, #40]
	subs	x8, x8, x9
	b.ge	LBB2_4
	b	LBB2_2
LBB2_2:                                 ;   in Loop: Header=BB2_1 Depth=1
	ldr	x8, [sp, #32]
	ldr	x9, [sp, #24]
	str	x8, [x9]
	ldr	x8, [sp, #24]
	add	x8, x8, #8
	str	x8, [sp, #24]
	b	LBB2_3
LBB2_3:                                 ;   in Loop: Header=BB2_1 Depth=1
	ldr	x8, [sp, #8]
	add	x8, x8, #1
	str	x8, [sp, #8]
	b	LBB2_1
LBB2_4:
	ldr	x8, [sp, #24]
	; InlineAsm Start
	mov	x27, x8
	; InlineAsm End
	ldr	x0, [sp, #16]
	add	sp, sp, #48
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_create_float_array     ; -- Begin function min_caml_create_float_array
	.p2align	2
min_caml_create_float_array:            ; @"\01min_caml_create_float_array"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #48
	.cfi_def_cfa_offset 48
	str	x0, [sp, #40]
	str	d0, [sp, #32]
	; InlineAsm Start
	mov	x8, x27
	; InlineAsm End
	str	x8, [sp, #24]
	ldr	x8, [sp, #24]
	str	x8, [sp, #16]
	str	xzr, [sp, #8]
	b	LBB3_1
LBB3_1:                                 ; =>This Inner Loop Header: Depth=1
	ldr	x8, [sp, #8]
	ldr	x9, [sp, #40]
	subs	x8, x8, x9
	b.ge	LBB3_4
	b	LBB3_2
LBB3_2:                                 ;   in Loop: Header=BB3_1 Depth=1
	ldr	d0, [sp, #32]
	ldr	x8, [sp, #24]
	str	d0, [x8]
	ldr	x8, [sp, #24]
	add	x8, x8, #8
	str	x8, [sp, #24]
	b	LBB3_3
LBB3_3:                                 ;   in Loop: Header=BB3_1 Depth=1
	ldr	x8, [sp, #8]
	add	x8, x8, #1
	str	x8, [sp, #8]
	b	LBB3_1
LBB3_4:
	ldr	x8, [sp, #24]
	; InlineAsm Start
	mov	x27, x8
	; InlineAsm End
	ldr	x0, [sp, #16]
	add	sp, sp, #48
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_truncate               ; -- Begin function min_caml_truncate
	.p2align	2
min_caml_truncate:                      ; @"\01min_caml_truncate"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	fcvtzs	x0, d0
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_print_float            ; -- Begin function min_caml_print_float
	.p2align	2
min_caml_print_float:                   ; @"\01min_caml_print_float"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	mov	x8, sp
	str	d0, [x8]
	adrp	x0, l_.str.2@PAGE
	add	x0, x0, l_.str.2@PAGEOFF
	bl	_printf
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_print_byte             ; -- Begin function min_caml_print_byte
	.p2align	2
min_caml_print_byte:                    ; @"\01min_caml_print_byte"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	str	x0, [sp, #8]
	ldr	x8, [sp, #8]
	mov	x0, x8
	bl	_putchar
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_read_int               ; -- Begin function min_caml_read_int
	.p2align	2
min_caml_read_int:                      ; @"\01min_caml_read_int"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	x9, sp
	add	x8, sp, #8
	str	x8, [x9]
	adrp	x0, l_.str@PAGE
	add	x0, x0, l_.str@PAGEOFF
	bl	_scanf
	ldr	x0, [sp, #8]
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_read_float             ; -- Begin function min_caml_read_float
	.p2align	2
min_caml_read_float:                    ; @"\01min_caml_read_float"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	x9, sp
	add	x8, sp, #8
	str	x8, [x9]
	adrp	x0, l_.str.2@PAGE
	add	x0, x0, l_.str.2@PAGEOFF
	bl	_scanf
	ldr	d0, [sp, #8]
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_atan                   ; -- Begin function min_caml_atan
	.p2align	2
min_caml_atan:                          ; @"\01min_caml_atan"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	bl	_atan
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_cos                    ; -- Begin function min_caml_cos
	.p2align	2
min_caml_cos:                           ; @"\01min_caml_cos"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	bl	_cos
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_floor                  ; -- Begin function min_caml_floor
	.p2align	2
min_caml_floor:                         ; @"\01min_caml_floor"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	frintm	d0, d0
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_sin                    ; -- Begin function min_caml_sin
	.p2align	2
min_caml_sin:                           ; @"\01min_caml_sin"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	bl	_sin
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_abs_float              ; -- Begin function min_caml_abs_float
	.p2align	2
min_caml_abs_float:                     ; @"\01min_caml_abs_float"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	fabs	d0, d0
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_float_of_int           ; -- Begin function min_caml_float_of_int
	.p2align	2
min_caml_float_of_int:                  ; @"\01min_caml_float_of_int"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x0, [sp, #8]
	ldr	d0, [sp, #8]
	scvtf	d0, d0
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_int_of_float           ; -- Begin function min_caml_int_of_float
	.p2align	2
min_caml_int_of_float:                  ; @"\01min_caml_int_of_float"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	fcvtzs	x0, d0
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	min_caml_sqrt                   ; -- Begin function min_caml_sqrt
	.p2align	2
min_caml_sqrt:                          ; @"\01min_caml_sqrt"
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	d0, [sp, #8]
	ldr	d0, [sp, #8]
	fsqrt	d0, d0
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_.str:                                 ; @.str
	.asciz	"%ld"

l_.str.1:                               ; @.str.1
	.asciz	"\n"

l_.str.2:                               ; @.str.2
	.asciz	"%lf"

.subsections_via_symbols
