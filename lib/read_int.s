# read an integer
# Autor: Cheung Kwan Wong  2010-2011

.section .data
   prompt_str2:
      .ascii "Enter integer: "
   pstr_end2:
      .set STR_SIZE, pstr_end2 - prompt_str2
 
 .section .bss

   .macro write str, str_size 
      movl  $4, %eax
      movl  $1, %ebx
      movl  \str, %ecx
      movl  \str_size, %edx
      int   $0x80
   .endm

   .macro read buff, buff_size
      movl  $3, %eax
      movl  $0, %ebx
      movl  \buff, %ecx
      movl  \buff_size, %edx
      int   $0x80
   .endm
   
.section .text
.globl read_int

# read_int(char value) -- print value followed by \n to stdout.
read_int: 
	pushl %ebp 
	movl %esp, %ebp 
	# initialize local variables:
	.equ base, -4 
        pushl $10 # base = 10
	.equ value, -8 
        pushl $0 # base = 10
	.equ negative, -12
		pushl $0 # negative = 0
	write $prompt_str2, $STR_SIZE
	
	pushl $0			#prepare buffer
	read %esp $1		#read one char
	cmpl $10,(%esp)		#compare to ASCII(endline)
	je .end
	
	cmpl $45,(%esp)		#check if number is negative
	jne .not_neg
	movl $1, negative(%ebp)	#if negative
	popl %eax
	jmp .loopr
.not_neg:					#if not negative
	subl $48, (%esp)
	popl %eax
	movl %eax, value(%ebp)
.loopr:						#loops till endline
	pushl $0
	read %esp $1			
	cmpl $10,(%esp)
	je .end
	subl $48, (%esp)
	popl %eax
	movl value(%ebp), %edx
	imull $10, %edx
	addl %eax, %edx
	movl %edx, value(%ebp)
	jmp .loopr
	
.end:						
	movl value(%ebp), %eax
	cmpl $0,negative(%ebp)	
	je .end_end
	negl %eax				#if negative
	
.end_end:	
	movl %ebp, %esp
	popl %ebp 				# restore saved frame pointer 
	ret
