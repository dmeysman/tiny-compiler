# read an integer
# Autor: Cheung Kwan Wong  2010-2011

.section .data
   prompt_str2rc:
      .ascii "Enter character: "
   pstr_end2rc:
      .set STR_SIZErc, pstr_end2rc - prompt_str2rc
 
 .section .bss

   .macro writerc str, str_size 
      movl  $4, %eax
      movl  $1, %ebx
      movl  \str, %ecx
      movl  \str_size, %edx
      int   $0x80
   .endm

   .macro readrc buff, buff_size
      movl  $3, %eax
      movl  $0, %ebx
      movl  \buff, %ecx
      movl  \buff_size, %edx
      int   $0x80
   .endm
   
.section .text
.globl read_char

# read_int(char value) -- print value followed by \n to stdout.
read_char: 
	pushl %ebp 
	movl %esp, %ebp 
	# initialize local variables:
	.equ base, -4 
        pushl $10 # base = 10
	.equ value, -8 
        pushl $0 # base = 10
	.equ negative, -12
		pushl $0 # negative = 0
	writerc $prompt_str2rc, $STR_SIZErc
	
	pushl $0			#prepare buffer
	read %esp $1		#read one char
	movl %edx, value(%ebp)
	

	popl %eax
	movl value(%ebp), %edx

	movl %eax, %edx
	movl %edx, value(%ebp)

.endrc:						
	movl value(%ebp), %eax
			#if negative
	
.end_endrc:	
	movl %ebp, %esp
	popl %ebp 				# restore saved frame pointer 
	ret
