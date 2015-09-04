.section .text
.globl print_char
.type print_char, @function
print_char:
	pushl %ebp
	movl %esp, %ebp
 movl $1, %edx # length of character to print, 1
 movl 8(%ebp), %ecx # what I want printed
 movl $1, %ebx # file descriptor for STDOUT
 movl $4, %eax # syscall number for sys_write

 int $0x80     # calls kernel
	movl %ebp, %esp
	popl %ebp
	ret
