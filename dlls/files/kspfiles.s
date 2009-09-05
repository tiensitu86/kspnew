	.file "kspfiles.lpr"
# Begin asmlist al_begin
# End asmlist al_begin
# Begin asmlist al_stabs
# End asmlist al_stabs
# Begin asmlist al_procedures

.section .text
	.balign 8,0x90
.globl	P$KSPFILES_ISSTREAM$ANSISTRING$$BOOLEAN
	.type	P$KSPFILES_ISSTREAM$ANSISTRING$$BOOLEAN,@function
P$KSPFILES_ISSTREAM$ANSISTRING$$BOOLEAN:
.Lc1:
	pushq	%rbp
.Lc3:
.Lc4:
	movq	%rsp,%rbp
.Lc5:
	subq	$144,%rsp
	movq	%rdi,-8(%rbp)
	movq	$0,-24(%rbp)
	movq	$0,-32(%rbp)
	movq	$0,-144(%rbp)
	movq	-8(%rbp),%rdi
	call	FPC_ANSISTR_INCR_REF@PLT
	leaq	-64(%rbp),%rdx
	leaq	-128(%rbp),%rsi
	movq	$1,%rdi
	call	FPC_PUSHEXCEPTADDR@PLT
	movq	%rax,%rdi
	call	FPC_SETJMP@PLT
	movq	%rax,-136(%rbp)
	testl	%eax,%eax
	jne	.Lj5
	leaq	-144(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-144(%rbp)
	movq	-8(%rbp),%rdi
	movq	$7,%rdx
	movq	$1,%rsi
	call	fpc_ansistr_copy@PLT
	movq	%rax,-144(%rbp)
	movq	-144(%rbp),%rdi
	call	FPC_ANSISTR_INCR_REF@PLT
	leaq	-24(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	-144(%rbp),%rax
	movq	%rax,-24(%rbp)
	leaq	-144(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-144(%rbp)
	movq	-8(%rbp),%rdi
	movq	$6,%rdx
	movq	$1,%rsi
	call	fpc_ansistr_copy@PLT
	movq	%rax,-144(%rbp)
	movq	-144(%rbp),%rdi
	call	FPC_ANSISTR_INCR_REF@PLT
	leaq	-32(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	-144(%rbp),%rax
	movq	%rax,-32(%rbp)
	movq	_$KSPFILES$_Ld1@GOTPCREL(%rip),%rax
	movq	(%rax),%rsi
	movq	-24(%rbp),%rdi
	call	fpc_ansistr_compare_equal@PLT
	testq	%rax,%rax
	je	.Lj24
	jmp	.Lj27
.Lj27:
	movq	_$KSPFILES$_Ld3@GOTPCREL(%rip),%rax
	movq	(%rax),%rsi
	movq	-32(%rbp),%rdi
	call	fpc_ansistr_compare_equal@PLT
	testq	%rax,%rax
	je	.Lj24
	jmp	.Lj26
.Lj26:
	movq	_$KSPFILES$_Ld5@GOTPCREL(%rip),%rax
	movq	(%rax),%rsi
	movq	-32(%rbp),%rdi
	call	fpc_ansistr_compare_equal@PLT
	testq	%rax,%rax
	je	.Lj24
	jmp	.Lj25
.Lj24:
	movb	$1,-12(%rbp)
	jmp	.Lj40
.Lj25:
	movb	$0,-12(%rbp)
.Lj40:
.Lj5:
	call	FPC_POPADDRSTACK@PLT
	leaq	-144(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-144(%rbp)
	leaq	-24(%rbp),%rdi
	call	fpc_ansistr_decr_ref@PLT
	movq	$0,-24(%rbp)
	leaq	-32(%rbp),%rdi
	call	fpc_ansistr_decr_ref@PLT
	movq	$0,-32(%rbp)
	leaq	-8(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	-136(%rbp),%rax
	testq	%rax,%rax
	je	.Lj6
	call	FPC_RERAISE@PLT
.Lj6:
	movb	-12(%rbp),%al
	leave
	ret
.Lc2:
.Le0:
	.size	P$KSPFILES_ISSTREAM$ANSISTRING$$BOOLEAN, .Le0 - P$KSPFILES_ISSTREAM$ANSISTRING$$BOOLEAN

.section .text
	.balign 8,0x90
.globl	P$KSPFILES_ISCD$ANSISTRING$$BOOLEAN
	.type	P$KSPFILES_ISCD$ANSISTRING$$BOOLEAN,@function
P$KSPFILES_ISCD$ANSISTRING$$BOOLEAN:
.Lc6:
	pushq	%rbp
.Lc8:
.Lc9:
	movq	%rsp,%rbp
.Lc10:
	subq	$128,%rsp
	movq	%rdi,-8(%rbp)
	movq	$0,-128(%rbp)
	movq	-8(%rbp),%rdi
	call	FPC_ANSISTR_INCR_REF@PLT
	leaq	-48(%rbp),%rdx
	leaq	-112(%rbp),%rsi
	movq	$1,%rdi
	call	FPC_PUSHEXCEPTADDR@PLT
	movq	%rax,%rdi
	call	FPC_SETJMP@PLT
	movq	%rax,-120(%rbp)
	testl	%eax,%eax
	jne	.Lj55
	leaq	-128(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-128(%rbp)
	movq	-8(%rbp),%rdi
	call	SYSUTILS_UPPERCASE$ANSISTRING$$ANSISTRING@PLT
	movq	%rax,-128(%rbp)
	movq	-128(%rbp),%rsi
	movq	_$KSPFILES$_Ld7@GOTPCREL(%rip),%rax
	movq	(%rax),%rdi
	call	SYSTEM_POS$ANSISTRING$ANSISTRING$$INT64@PLT
	cmpq	$1,%rax
	seteb	-12(%rbp)
.Lj55:
	call	FPC_POPADDRSTACK@PLT
	leaq	-128(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-128(%rbp)
	leaq	-8(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	-120(%rbp),%rax
	testq	%rax,%rax
	je	.Lj56
	call	FPC_RERAISE@PLT
.Lj56:
	movb	-12(%rbp),%al
	leave
	ret
.Lc7:
.Le1:
	.size	P$KSPFILES_ISCD$ANSISTRING$$BOOLEAN, .Le1 - P$KSPFILES_ISCD$ANSISTRING$$BOOLEAN

.section .text
	.balign 8,0x90
.globl	P$KSPFILES_PRODUCEFORMATEDSTRING$SHORTSTRING$TID3TAG$LONGWORD$LONGINT$$SHORTSTRING
	.type	P$KSPFILES_PRODUCEFORMATEDSTRING$SHORTSTRING$TID3TAG$LONGWORD$LONGINT$$SHORTSTRING,@function
P$KSPFILES_PRODUCEFORMATEDSTRING$SHORTSTRING$TID3TAG$LONGWORD$LONGINT$$SHORTSTRING:
.Lc11:
	pushq	%rbp
.Lc13:
.Lc14:
	movq	%rsp,%rbp
.Lc15:
	subq	$960,%rsp
	movq	%rbx,-960(%rbp)
	movq	%rdi,-8(%rbp)
	movl	%esi,-16(%rbp)
	movl	%edx,-24(%rbp)
	movq	%rcx,-32(%rbp)
	movq	-8(%rbp),%rsi
	leaq	-548(%rbp),%rdx
	movq	$255,%rdi
	call	FPC_SHORTSTR_ASSIGN@PLT
	movq	$0,-920(%rbp)
	leaq	-592(%rbp),%rdx
	leaq	-656(%rbp),%rsi
	movq	$1,%rdi
	call	FPC_PUSHEXCEPTADDR@PLT
	movq	%rax,%rdi
	call	FPC_SETJMP@PLT
	movq	%rax,-8(%rbp)
	testl	%eax,%eax
	jne	.Lj68
	leaq	-548(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$255,%rsi
	call	fpc_shortstr_to_shortstr@PLT
	movq	_$KSPFILES$_Ld9@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj83
	jmp	.Lj84
.Lj83:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$9,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	leaq	273(%rbp),%rdi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj84:
	movq	_$KSPFILES$_Ld10@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj107
	jmp	.Lj108
.Lj107:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$8,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	leaq	529(%rbp),%rdi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj108:
	movq	_$KSPFILES$_Ld11@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj131
	jmp	.Lj132
.Lj131:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$8,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	leaq	17(%rbp),%rdi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj132:
	movq	_$KSPFILES$_Ld12@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj155
	jmp	.Lj156
.Lj155:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$8,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	leaq	1297(%rbp),%rdi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj156:
	movq	_$KSPFILES$_Ld13@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj179
	jmp	.Lj180
.Lj179:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$7,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	leaq	785(%rbp),%rdi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj180:
	movq	_$KSPFILES$_Ld14@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj203
	jmp	.Lj204
.Lj203:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$8,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	leaq	-912(%rbp),%rbx
	leaq	-920(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-920(%rbp)
	movl	1553(%rbp),%edi
	call	SYSUTILS_INTTOSTR$LONGINT$$ANSISTRING@PLT
	movq	%rax,-920(%rbp)
	movq	-920(%rbp),%rdx
	movq	%rbx,%rdi
	movq	$255,%rsi
	call	fpc_ansistr_to_shortstr@PLT
	leaq	-912(%rbp),%rdi
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj204:
	movq	_$KSPFILES$_Ld15@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj237
	jmp	.Lj238
.Lj237:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$10,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	leaq	1041(%rbp),%rdi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj238:
	movq	_$KSPFILES$_Ld16@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj261
	jmp	.Lj262
.Lj261:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$10,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	leaq	-912(%rbp),%rbx
	leaq	-920(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-920(%rbp)
	movl	-24(%rbp),%edi
	call	SYSUTILS_INTTOSTR$LONGINT$$ANSISTRING@PLT
	movq	%rax,-920(%rbp)
	movq	-920(%rbp),%rdx
	movq	%rbx,%rdi
	movq	$255,%rsi
	call	fpc_ansistr_to_shortstr@PLT
	leaq	-912(%rbp),%rdi
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj262:
	movq	_$KSPFILES$_Ld17@GOTPCREL(%rip),%rdi
	leaq	-288(%rbp),%rsi
	call	SYSTEM_POS$SHORTSTRING$SHORTSTRING$$INT64@PLT
	movl	%eax,-292(%rbp)
	movl	-292(%rbp),%eax
	cmpl	$0,%eax
	jg	.Lj295
	jmp	.Lj296
.Lj295:
	movslq	-292(%rbp),%rdx
	leaq	-288(%rbp),%rdi
	movq	$9,%rcx
	movq	$255,%rsi
	call	SYSTEM_DELETE$OPENSTRING$INT64$INT64@PLT
	movl	-16(%rbp),%eax
	cmpq	$3600000,%rax
	jg	.Lj305
	jmp	.Lj306
.Lj305:
	leaq	-912(%rbp),%rbx
	leaq	-920(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-920(%rbp)
	movl	-16(%rbp),%eax
	movl	%eax,-928(%rbp)
	movl	$0,-924(%rbp)
	fildq	-928(%rbp)
	movq	_$KSPFILES$_Ld18@GOTPCREL(%rip),%rax
	fldt	(%rax)
	fdivrp	%st,%st(1)
	fstpl	-928(%rbp)
	movsd	-928(%rbp),%xmm0
	movq	_$KSPFILES$_Ld19@GOTPCREL(%rip),%rax
	movq	(%rax),%rdi
	movsd	%xmm0,%xmm0
	call	SYSUTILS_FORMATDATETIME$ANSISTRING$TDATETIME$$ANSISTRING@PLT
	movq	%rax,-920(%rbp)
	movq	-920(%rbp),%rdx
	movq	%rbx,%rdi
	movq	$255,%rsi
	call	fpc_ansistr_to_shortstr@PLT
	leaq	-912(%rbp),%rdi
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
	jmp	.Lj327
.Lj306:
	leaq	-912(%rbp),%rbx
	leaq	-920(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-920(%rbp)
	movl	-16(%rbp),%eax
	movl	%eax,-928(%rbp)
	movl	$0,-924(%rbp)
	fildq	-928(%rbp)
	movq	_$KSPFILES$_Ld18@GOTPCREL(%rip),%rax
	fldt	(%rax)
	fdivrp	%st,%st(1)
	fstpl	-928(%rbp)
	movsd	-928(%rbp),%xmm0
	movq	_$KSPFILES$_Ld21@GOTPCREL(%rip),%rax
	movq	(%rax),%rdi
	movsd	%xmm0,%xmm0
	call	SYSUTILS_FORMATDATETIME$ANSISTRING$TDATETIME$$ANSISTRING@PLT
	movq	%rax,-920(%rbp)
	movq	-920(%rbp),%rdx
	movq	%rbx,%rdi
	movq	$255,%rsi
	call	fpc_ansistr_to_shortstr@PLT
	leaq	-912(%rbp),%rdi
	movslq	-292(%rbp),%rcx
	leaq	-288(%rbp),%rsi
	movq	$255,%rdx
	call	SYSTEM_INSERT$SHORTSTRING$OPENSTRING$INT64@PLT
.Lj327:
.Lj296:
	leaq	-288(%rbp),%rdx
	movq	-32(%rbp),%rdi
	movq	$255,%rsi
	call	fpc_shortstr_to_shortstr@PLT
.Lj68:
	call	FPC_POPADDRSTACK@PLT
	leaq	-920(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-920(%rbp)
	movq	-8(%rbp),%rax
	testq	%rax,%rax
	je	.Lj69
	call	FPC_RERAISE@PLT
.Lj69:
	movq	-960(%rbp),%rbx
	leave
	ret
.Lc12:
.Le2:
	.size	P$KSPFILES_PRODUCEFORMATEDSTRING$SHORTSTRING$TID3TAG$LONGWORD$LONGINT$$SHORTSTRING, .Le2 - P$KSPFILES_PRODUCEFORMATEDSTRING$SHORTSTRING$TID3TAG$LONGWORD$LONGINT$$SHORTSTRING

.section .text
	.balign 8,0x90
.globl	P$KSPFILES_REMOVEFORBIDDENCHARS$ANSISTRING$CHAR
	.type	P$KSPFILES_REMOVEFORBIDDENCHARS$ANSISTRING$CHAR,@function
P$KSPFILES_REMOVEFORBIDDENCHARS$ANSISTRING$CHAR:
.Lc16:
	pushq	%rbp
.Lc18:
.Lc19:
	movq	%rsp,%rbp
.Lc20:
	subq	$32,%rsp
	movq	%rbx,-32(%rbp)
	movq	%rdi,-8(%rbp)
	movb	%sil,-16(%rbp)
	movq	-8(%rbp),%rax
	movq	(%rax),%rax
	testq	%rax,%rax
	je	.Lj356
	jmp	.Lj357
.Lj356:
	jmp	.Lj354
.Lj357:
	movq	-8(%rbp),%rax
	movq	(%rax),%rbx
	testq	%rbx,%rbx
	je	.Lj361
	movq	-8(%rbx),%rbx
.Lj361:
	movl	$1,-20(%rbp)
	cmpl	-20(%rbp),%ebx
	jl	.Lj359
	decl	-20(%rbp)
	.balign 4,0x90
.Lj360:
	incl	-20(%rbp)
	movq	-8(%rbp),%rax
	movq	(%rax),%rdx
	movslq	-20(%rbp),%rax
	movb	-1(%rdx,%rax),%dl
	cmpb	$34,%dl
	jb	.Lj363
	movb	%dl,%al
	subb	$34,%dl
	cmpb	$34,%al
	je	.Lj364
	movb	%dl,%al
	subb	$8,%dl
	cmpb	$8,%al
	je	.Lj364
	movb	%dl,%al
	subb	$5,%dl
	cmpb	$5,%al
	je	.Lj364
	movb	%dl,%al
	subb	$11,%dl
	cmpb	$11,%al
	je	.Lj364
	movb	%dl,%al
	subb	$2,%dl
	cmpb	$2,%al
	je	.Lj364
	movb	%dl,%al
	subb	$2,%dl
	cmpb	$2,%al
	jb	.Lj363
	movb	%dl,%al
	decb	%dl
	cmpb	$1,%al
	jbe	.Lj364
	movb	%dl,%al
	subb	$29,%dl
	cmpb	$29,%al
	je	.Lj364
	movb	%dl,%al
	subb	$32,%dl
	cmpb	$32,%al
	je	.Lj364
	jmp	.Lj363
.Lj364:
	movq	-8(%rbp),%rdi
	call	fpc_ansistr_unique@PLT
	movslq	-20(%rbp),%rdx
	movb	-16(%rbp),%cl
	movb	%cl,-1(%rax,%rdx)
	jmp	.Lj362
.Lj363:
.Lj362:
	cmpl	-20(%rbp),%ebx
	jg	.Lj360
.Lj359:
.Lj354:
	movq	-32(%rbp),%rbx
	leave
	ret
.Lc17:
.Le3:
	.size	P$KSPFILES_REMOVEFORBIDDENCHARS$ANSISTRING$CHAR, .Le3 - P$KSPFILES_REMOVEFORBIDDENCHARS$ANSISTRING$CHAR

.section .text
	.balign 8,0x90
.globl	P$KSPFILES_WRITECHANGEFILE$TFILERENAMED
	.type	P$KSPFILES_WRITECHANGEFILE$TFILERENAMED,@function
P$KSPFILES_WRITECHANGEFILE$TFILERENAMED:
.Lc21:
	pushq	%rbp
.Lc23:
.Lc24:
	movq	%rsp,%rbp
.Lc25:
	subq	$784,%rsp
	movq	%rbx,-784(%rbp)
	movq	%r12,-776(%rbp)
	movq	$0,-760(%rbp)
	movq	$0,-752(%rbp)
	movq	$0,-744(%rbp)
	leaq	-400(%rbp),%rdx
	leaq	-464(%rbp),%rsi
	movq	$1,%rdi
	call	FPC_PUSHEXCEPTADDR@PLT
	movq	%rax,%rdi
	call	FPC_SETJMP@PLT
	movq	%rax,-472(%rbp)
	testl	%eax,%eax
	jne	.Lj371
	leaq	-736(%rbp),%rbx
	leaq	-744(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-744(%rbp)
	movq	_$KSPFILES$_Ld23@GOTPCREL(%rip),%rax
	movq	(%rax),%r12
	leaq	-752(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-752(%rbp)
	leaq	-760(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-760(%rbp)
	movl	$0,%edi
	call	OBJPAS_PARAMSTR$LONGINT$$ANSISTRING@PLT
	movq	%rax,-760(%rbp)
	movq	-760(%rbp),%rdi
	call	SYSUTILS_EXTRACTFILEPATH$ANSISTRING$$ANSISTRING@PLT
	movq	%rax,-752(%rbp)
	movq	-752(%rbp),%rsi
	leaq	-744(%rbp),%rdi
	movq	%r12,%rdx
	call	fpc_ansistr_concat@PLT
	movq	-744(%rbp),%rdx
	movq	%rbx,%rdi
	movq	$255,%rsi
	call	fpc_ansistr_to_shortstr@PLT
	leaq	-736(%rbp),%rsi
	leaq	-368(%rbp),%rdi
	call	OBJPAS_ASSIGNFILE$TYPEDFILE$SHORTSTRING@PLT
	leaq	-760(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-760(%rbp)
	movq	_$KSPFILES$_Ld23@GOTPCREL(%rip),%rax
	movq	(%rax),%rbx
	leaq	-752(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-752(%rbp)
	leaq	-744(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-744(%rbp)
	movl	$0,%edi
	call	OBJPAS_PARAMSTR$LONGINT$$ANSISTRING@PLT
	movq	%rax,-744(%rbp)
	movq	-744(%rbp),%rdi
	call	SYSUTILS_EXTRACTFILEPATH$ANSISTRING$$ANSISTRING@PLT
	movq	%rax,-752(%rbp)
	movq	-752(%rbp),%rsi
	leaq	-760(%rbp),%rdi
	movq	%rbx,%rdx
	call	fpc_ansistr_concat@PLT
	movq	-760(%rbp),%rdi
	call	SYSUTILS_FILEEXISTS$ANSISTRING$$BOOLEAN@PLT
	testb	%al,%al
	jne	.Lj396
	jmp	.Lj397
.Lj396:
	leaq	-760(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-760(%rbp)
	movq	_$KSPFILES$_Ld23@GOTPCREL(%rip),%rax
	movq	(%rax),%rbx
	leaq	-752(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-752(%rbp)
	leaq	-744(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-744(%rbp)
	movl	$0,%edi
	call	OBJPAS_PARAMSTR$LONGINT$$ANSISTRING@PLT
	movq	%rax,-744(%rbp)
	movq	-744(%rbp),%rdi
	call	SYSUTILS_EXTRACTFILEPATH$ANSISTRING$$ANSISTRING@PLT
	movq	%rax,-752(%rbp)
	movq	-752(%rbp),%rsi
	leaq	-760(%rbp),%rdi
	movq	%rbx,%rdx
	call	fpc_ansistr_concat@PLT
	movq	-760(%rbp),%rdi
	call	SYSUTILS_DELETEFILE$ANSISTRING$$BOOLEAN@PLT
.Lj397:
	leaq	-368(%rbp),%rdi
	movl	$8194,%esi
	call	fpc_rewrite_typed@PLT
	call	FPC_IOCHECK@PLT
	leaq	16(%rbp),%rdx
	leaq	-368(%rbp),%rsi
	movl	$8194,%edi
	call	fpc_typed_write@PLT
	call	FPC_IOCHECK@PLT
	leaq	-368(%rbp),%rdi
	call	OBJPAS_CLOSEFILE$file@PLT
	call	FPC_IOCHECK@PLT
.Lj371:
	call	FPC_POPADDRSTACK@PLT
	leaq	-760(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-760(%rbp)
	leaq	-752(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-752(%rbp)
	leaq	-744(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-744(%rbp)
	movq	-472(%rbp),%rax
	testq	%rax,%rax
	je	.Lj372
	call	FPC_RERAISE@PLT
.Lj372:
	movq	-784(%rbp),%rbx
	movq	-776(%rbp),%r12
	leave
	ret
.Lc22:
.Le4:
	.size	P$KSPFILES_WRITECHANGEFILE$TFILERENAMED, .Le4 - P$KSPFILES_WRITECHANGEFILE$TFILERENAMED

.section .text
	.balign 8,0x90
.globl	P$KSPFILES_READCHANGEFILE$$TFILERENAMED
	.type	P$KSPFILES_READCHANGEFILE$$TFILERENAMED,@function
P$KSPFILES_READCHANGEFILE$$TFILERENAMED:
.Lc26:
	pushq	%rbp
.Lc28:
.Lc29:
	movq	%rsp,%rbp
.Lc30:
	subq	$800,%rsp
	movq	%rbx,-800(%rbp)
	movq	%r12,-792(%rbp)
	movq	%rdi,-8(%rbp)
	movq	$0,-776(%rbp)
	movq	$0,-768(%rbp)
	movq	$0,-760(%rbp)
	leaq	-416(%rbp),%rdx
	leaq	-480(%rbp),%rsi
	movq	$1,%rdi
	call	FPC_PUSHEXCEPTADDR@PLT
	movq	%rax,%rdi
	call	FPC_SETJMP@PLT
	movq	%rax,-488(%rbp)
	testl	%eax,%eax
	jne	.Lj436
	leaq	-752(%rbp),%rbx
	leaq	-760(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-760(%rbp)
	movq	_$KSPFILES$_Ld23@GOTPCREL(%rip),%rax
	movq	(%rax),%r12
	leaq	-768(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-768(%rbp)
	leaq	-776(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-776(%rbp)
	movl	$0,%edi
	call	OBJPAS_PARAMSTR$LONGINT$$ANSISTRING@PLT
	movq	%rax,-776(%rbp)
	movq	-776(%rbp),%rdi
	call	SYSUTILS_EXTRACTFILEPATH$ANSISTRING$$ANSISTRING@PLT
	movq	%rax,-768(%rbp)
	movq	-768(%rbp),%rsi
	leaq	-760(%rbp),%rdi
	movq	%r12,%rdx
	call	fpc_ansistr_concat@PLT
	movq	-760(%rbp),%rdx
	movq	%rbx,%rdi
	movq	$255,%rsi
	call	fpc_ansistr_to_shortstr@PLT
	leaq	-752(%rbp),%rsi
	leaq	-384(%rbp),%rdi
	call	OBJPAS_ASSIGNFILE$TYPEDFILE$SHORTSTRING@PLT
	leaq	-776(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-776(%rbp)
	movq	_$KSPFILES$_Ld23@GOTPCREL(%rip),%rax
	movq	(%rax),%rbx
	leaq	-768(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-768(%rbp)
	leaq	-760(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-760(%rbp)
	movl	$0,%edi
	call	OBJPAS_PARAMSTR$LONGINT$$ANSISTRING@PLT
	movq	%rax,-760(%rbp)
	movq	-760(%rbp),%rdi
	call	SYSUTILS_EXTRACTFILEPATH$ANSISTRING$$ANSISTRING@PLT
	movq	%rax,-768(%rbp)
	movq	-768(%rbp),%rsi
	leaq	-776(%rbp),%rdi
	movq	%rbx,%rdx
	call	fpc_ansistr_concat@PLT
	movq	-776(%rbp),%rdi
	call	SYSUTILS_FILEEXISTS$ANSISTRING$$BOOLEAN@PLT
	testb	%al,%al
	je	.Lj461
	jmp	.Lj462
.Lj461:
	jmp	.Lj436
.Lj462:
	leaq	-384(%rbp),%rdi
	movl	$8194,%esi
	call	fpc_reset_typed@PLT
	call	FPC_IOCHECK@PLT
	movq	-8(%rbp),%rdx
	leaq	-384(%rbp),%rsi
	movl	$8194,%edi
	call	fpc_typed_read@PLT
	call	FPC_IOCHECK@PLT
	leaq	-384(%rbp),%rdi
	call	OBJPAS_CLOSEFILE$file@PLT
	call	FPC_IOCHECK@PLT
	leaq	-776(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-776(%rbp)
	movq	_$KSPFILES$_Ld23@GOTPCREL(%rip),%rax
	movq	(%rax),%rbx
	leaq	-768(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-768(%rbp)
	leaq	-760(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-760(%rbp)
	movl	$0,%edi
	call	OBJPAS_PARAMSTR$LONGINT$$ANSISTRING@PLT
	movq	%rax,-760(%rbp)
	movq	-760(%rbp),%rdi
	call	SYSUTILS_EXTRACTFILEPATH$ANSISTRING$$ANSISTRING@PLT
	movq	%rax,-768(%rbp)
	movq	-768(%rbp),%rsi
	leaq	-776(%rbp),%rdi
	movq	%rbx,%rdx
	call	fpc_ansistr_concat@PLT
	movq	-776(%rbp),%rdi
	call	SYSUTILS_DELETEFILE$ANSISTRING$$BOOLEAN@PLT
.Lj436:
	call	FPC_POPADDRSTACK@PLT
	leaq	-776(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-776(%rbp)
	leaq	-768(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-768(%rbp)
	leaq	-760(%rbp),%rdi
	call	FPC_ANSISTR_DECR_REF@PLT
	movq	$0,-760(%rbp)
	movq	-488(%rbp),%rax
	testq	%rax,%rax
	je	.Lj437
	call	FPC_RERAISE@PLT
.Lj437:
	movq	-800(%rbp),%rbx
	movq	-792(%rbp),%r12
	leave
	ret
.Lc27:
.Le5:
	.size	P$KSPFILES_READCHANGEFILE$$TFILERENAMED, .Le5 - P$KSPFILES_READCHANGEFILE$$TFILERENAMED

.section .fpc
	.quad	FPC_LIB_EXIT

.section .text
	.balign 8,0x90
.globl	PASCALMAIN
	.type	PASCALMAIN,@function
PASCALMAIN:
.globl	P$KSPFILES_main
	.type	P$KSPFILES_main,@function
P$KSPFILES_main:
.Lc31:
	pushq	%rbp
.Lc33:
.Lc34:
	movq	%rsp,%rbp
.Lc35:
	call	FPC_INITIALIZEUNITS@PLT
	leave
	ret
.Lc32:
.Le6:
	.size	P$KSPFILES_main, .Le6 - P$KSPFILES_main

.section .text
	.balign 8,0x90
.globl	RemoveForbiddenChars
	.type	RemoveForbiddenChars,@function
RemoveForbiddenChars:
	jmp	P$KSPFILES_REMOVEFORBIDDENCHARS$ANSISTRING$CHAR@PLT
.Le7:
	.size	RemoveForbiddenChars, .Le7 - RemoveForbiddenChars
	.balign 8,0x90
.globl	ProduceFormatedString
	.type	ProduceFormatedString,@function
ProduceFormatedString:
	jmp	P$KSPFILES_PRODUCEFORMATEDSTRING$SHORTSTRING$TID3TAG$LONGWORD$LONGINT$$SHORTSTRING@PLT
.Le8:
	.size	ProduceFormatedString, .Le8 - ProduceFormatedString
	.balign 8,0x90
.globl	IsStream
	.type	IsStream,@function
IsStream:
	jmp	P$KSPFILES_ISSTREAM$ANSISTRING$$BOOLEAN@PLT
.Le9:
	.size	IsStream, .Le9 - IsStream
	.balign 8,0x90
.globl	IsCD
	.type	IsCD,@function
IsCD:
	jmp	P$KSPFILES_ISCD$ANSISTRING$$BOOLEAN@PLT
.Le10:
	.size	IsCD, .Le10 - IsCD
	.balign 8,0x90
.globl	ReadChangeFile
	.type	ReadChangeFile,@function
ReadChangeFile:
	jmp	P$KSPFILES_READCHANGEFILE$$TFILERENAMED@PLT
.Le11:
	.size	ReadChangeFile, .Le11 - ReadChangeFile
	.balign 8,0x90
.globl	WriteChangeFile
	.type	WriteChangeFile,@function
WriteChangeFile:
	jmp	P$KSPFILES_WRITECHANGEFILE$TFILERENAMED@PLT
.Le12:
	.size	WriteChangeFile, .Le12 - WriteChangeFile
# End asmlist al_procedures
# Begin asmlist al_globals

.section .data.rel
	.balign 8
.globl	THREADVARLIST_P$KSPFILES
	.type	THREADVARLIST_P$KSPFILES,@object
THREADVARLIST_P$KSPFILES:
	.quad	0
.Le13:
	.size	THREADVARLIST_P$KSPFILES, .Le13 - THREADVARLIST_P$KSPFILES

.section .data.rel
	.balign 8
.globl	INITFINAL
	.type	INITFINAL,@object
INITFINAL:
	.long	7,0
	.quad	INIT$_SYSTEM
	.quad	0
	.quad	INIT$_OBJPAS
	.quad	FINALIZE$_OBJPAS
	.quad	INIT$_CMEM
	.quad	FINALIZE$_CMEM
	.quad	INIT$_UNIX
	.quad	FINALIZE$_UNIX
	.quad	INIT$_SYSUTILS
	.quad	FINALIZE$_SYSUTILS
	.quad	INIT$_TYPINFO
	.quad	FINALIZE$_TYPINFO
	.quad	INIT$_CLASSES
	.quad	FINALIZE$_CLASSES
.Le14:
	.size	INITFINAL, .Le14 - INITFINAL

.section .data.rel
	.balign 8
.globl	FPC_THREADVARTABLES
	.type	FPC_THREADVARTABLES,@object
FPC_THREADVARTABLES:
	.long	17
	.quad	THREADVARLIST_SYSTEM
	.quad	THREADVARLIST_OBJPAS
	.quad	THREADVARLIST_CMEM
	.quad	THREADVARLIST_UNIXTYPE
	.quad	THREADVARLIST_BASEUNIX
	.quad	THREADVARLIST_UNIXUTIL
	.quad	THREADVARLIST_SYSCALL
	.quad	THREADVARLIST_UNIX
	.quad	THREADVARLIST_ERRORS
	.quad	THREADVARLIST_SYSCONST
	.quad	THREADVARLIST_SYSUTILS
	.quad	THREADVARLIST_TYPES
	.quad	THREADVARLIST_RTLCONSTS
	.quad	THREADVARLIST_TYPINFO
	.quad	THREADVARLIST_CLASSES
	.quad	THREADVARLIST_KSPMESSAGES
	.quad	THREADVARLIST_P$KSPFILES
.Le15:
	.size	FPC_THREADVARTABLES, .Le15 - FPC_THREADVARTABLES

.section .data.rel
	.balign 8
.globl	FPC_RESOURCESTRINGTABLES
	.type	FPC_RESOURCESTRINGTABLES,@object
FPC_RESOURCESTRINGTABLES:
	.quad	2
	.quad	RESSTR_SYSCONST_START
	.quad	RESSTR_SYSCONST_END
	.quad	RESSTR_RTLCONSTS_START
	.quad	RESSTR_RTLCONSTS_END
.Le16:
	.size	FPC_RESOURCESTRINGTABLES, .Le16 - FPC_RESOURCESTRINGTABLES

.section .fpc
	.balign 8
	.ascii	"FPC 2.2.4 [2009/04/13] for x86_64 - Linux"

.section .data.rel
	.balign 8
.globl	__stklen
	.type	__stklen,@object
__stklen:
	.quad	262144

.section .data.rel
	.balign 8
.globl	__heapsize
	.type	__heapsize,@object
__heapsize:
	.quad	0

.section .data.rel
.globl	__fpc_valgrind
	.type	__fpc_valgrind,@object
__fpc_valgrind:
	.byte	0
# End asmlist al_globals
# Begin asmlist al_const
# End asmlist al_const
# Begin asmlist al_typedconsts

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld1
_$KSPFILES$_Ld1:
	.quad	_$KSPFILES$_Ld2
	.quad	-1,7
.globl	_$KSPFILES$_Ld2
_$KSPFILES$_Ld2:
	.ascii	"http://\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld3
_$KSPFILES$_Ld3:
	.quad	_$KSPFILES$_Ld4
	.quad	-1,6
.globl	_$KSPFILES$_Ld4
_$KSPFILES$_Ld4:
	.ascii	"ftp://\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld5
_$KSPFILES$_Ld5:
	.quad	_$KSPFILES$_Ld6
	.quad	-1,6
.globl	_$KSPFILES$_Ld6
_$KSPFILES$_Ld6:
	.ascii	"mms://\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld7
_$KSPFILES$_Ld7:
	.quad	_$KSPFILES$_Ld8
	.quad	-1,6
.globl	_$KSPFILES$_Ld8
_$KSPFILES$_Ld8:
	.ascii	"CDA://\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld9
_$KSPFILES$_Ld9:
	.ascii	"\011[%artist]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld10
_$KSPFILES$_Ld10:
	.ascii	"\010[%album]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld11
_$KSPFILES$_Ld11:
	.ascii	"\010[%title]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld12
_$KSPFILES$_Ld12:
	.ascii	"\010[%genre]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld13
_$KSPFILES$_Ld13:
	.ascii	"\007[%year]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld14
_$KSPFILES$_Ld14:
	.ascii	"\010[%track]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld15
_$KSPFILES$_Ld15:
	.ascii	"\012[%comment]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld16
_$KSPFILES$_Ld16:
	.ascii	"\012[%plindex]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld17
_$KSPFILES$_Ld17:
	.ascii	"\011[%length]\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld18
_$KSPFILES$_Ld18:
	.byte	0,0,0,0,0,128,203,164,25,64

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld19
_$KSPFILES$_Ld19:
	.quad	_$KSPFILES$_Ld20
	.quad	-1,8
.globl	_$KSPFILES$_Ld20
_$KSPFILES$_Ld20:
	.ascii	"hh:nn:ss\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld21
_$KSPFILES$_Ld21:
	.quad	_$KSPFILES$_Ld22
	.quad	-1,5
.globl	_$KSPFILES$_Ld22
_$KSPFILES$_Ld22:
	.ascii	"nn:ss\000"

.section .data.rel
	.balign 8
.globl	_$KSPFILES$_Ld23
_$KSPFILES$_Ld23:
	.quad	_$KSPFILES$_Ld24
	.quad	-1,11
.globl	_$KSPFILES$_Ld24
_$KSPFILES$_Ld24:
	.ascii	"changes.fil\000"
# End asmlist al_typedconsts
# Begin asmlist al_rotypedconsts
# End asmlist al_rotypedconsts
# Begin asmlist al_threadvars
# End asmlist al_threadvars
# Begin asmlist al_imports
# End asmlist al_imports
# Begin asmlist al_exports
# End asmlist al_exports
# Begin asmlist al_resources
# End asmlist al_resources
# Begin asmlist al_rtti
# End asmlist al_rtti
# Begin asmlist al_dwarf

.section .debug_frame
.Lc36:
	.long	.Lc38-.Lc37
.Lc37:
	.long	-1
	.byte	1
	.byte	0
	.uleb128	1
	.sleb128	-4
	.byte	16
	.byte	12
	.uleb128	7
	.uleb128	8
	.byte	5
	.uleb128	16
	.uleb128	2
	.balign 4,0
.Lc38:
	.long	.Lc40-.Lc39
.Lc39:
	.long	.Lc36
	.quad	.Lc1
	.quad	.Lc2-.Lc1
	.byte	4
	.long	.Lc3-.Lc1
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc4-.Lc3
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc5-.Lc4
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc40:
	.long	.Lc42-.Lc41
.Lc41:
	.long	.Lc36
	.quad	.Lc6
	.quad	.Lc7-.Lc6
	.byte	4
	.long	.Lc8-.Lc6
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc9-.Lc8
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc10-.Lc9
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc42:
	.long	.Lc44-.Lc43
.Lc43:
	.long	.Lc36
	.quad	.Lc11
	.quad	.Lc12-.Lc11
	.byte	4
	.long	.Lc13-.Lc11
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc14-.Lc13
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc15-.Lc14
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc44:
	.long	.Lc46-.Lc45
.Lc45:
	.long	.Lc36
	.quad	.Lc16
	.quad	.Lc17-.Lc16
	.byte	4
	.long	.Lc18-.Lc16
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc19-.Lc18
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc20-.Lc19
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc46:
	.long	.Lc48-.Lc47
.Lc47:
	.long	.Lc36
	.quad	.Lc21
	.quad	.Lc22-.Lc21
	.byte	4
	.long	.Lc23-.Lc21
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc24-.Lc23
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc25-.Lc24
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc48:
	.long	.Lc50-.Lc49
.Lc49:
	.long	.Lc36
	.quad	.Lc26
	.quad	.Lc27-.Lc26
	.byte	4
	.long	.Lc28-.Lc26
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc29-.Lc28
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc30-.Lc29
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc50:
	.long	.Lc52-.Lc51
.Lc51:
	.long	.Lc36
	.quad	.Lc31
	.quad	.Lc32-.Lc31
	.byte	4
	.long	.Lc33-.Lc31
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc34-.Lc33
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc35-.Lc34
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc52:
# End asmlist al_dwarf
# Begin asmlist al_dwarf_info
# End asmlist al_dwarf_info
# Begin asmlist al_dwarf_abbrev
# End asmlist al_dwarf_abbrev
# Begin asmlist al_dwarf_line
# End asmlist al_dwarf_line
# Begin asmlist al_picdata
# End asmlist al_picdata
# Begin asmlist al_resourcestrings
# End asmlist al_resourcestrings
# Begin asmlist al_end
# End asmlist al_end

