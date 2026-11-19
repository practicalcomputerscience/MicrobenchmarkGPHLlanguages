; random_streams_for_perf_stats_x86-64-v1.asm
;
; 2026-11-19
;
; Assembly code for NASM (Netwide Assembler) for x86-64 Linux, System V ABI, systems.
; It uses only baseline x86-64 instructions.
;
;
; build on Ubuntu 24 LTS: $ nasm -f elf64 random_streams_for_perf_stats_x86-64-v1.asm -o random_streams_for_perf_stats_x86-64-v1.o
;                         $ gcc -no-pie random_streams_for_perf_stats_x86-64-v1.o -o random_streams_for_perf_stats_x86-64-v1
;
; run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_x86-64-v1
;
;                         generating a random bit stream...
;                         could not write to file: random_bitstring.bin: Permission denied
;                         Byte stream has been written to disk under name: random_bitstring.byte
;                         $ sudo perf stat -r 20 ./random_streams_for_perf_stats_x86-64-v1 => 0.0020...0.0037 <<<<<<<<
;
;
; $ nasm --version
; NASM version 2.16.01
; $ gcc --version
; gcc-14 (Ubuntu 14.2.0-4ubuntu2~24.04.1) 14.2.0
; ...
; $
;
;
; transpiled from random_streams_for_perf_stats.cpp with Duck.ai


default rel

global main

extern clock_gettime
extern fopen
extern fwrite
extern fclose
extern puts
extern perror

%define END       62501  ; 62501 for exactly 1M binary digits
%define VALUE_COUNT (END - 1)

%define BIN_SIZE  (VALUE_COUNT * 16)
%define HEX_SIZE  (VALUE_COUNT * 4)

%define MODULUS_m   65521  ; = 2^16 - 15
%define MULTIPLIER_a 17364
%define INCREMENT_c 0

%define SEED_MOD  (MODULUS_m - 2)

section .rodata
    file_bits_x   db "random_bitstring.bin", 0
    file_bits_hex db "random_bitstring.byte", 0
    file_mode     db "wb", 0

    start_msg_0   db "", 0  ; first, a newline is to be printed
    start_msg     db "generating a random bit stream...", 0

    bin_msg_ok      db "Bit stream has been written to disk under name:  random_bitstring.bin", 0
    hex_msg_ok      db "Byte stream has been written to disk under name: random_bitstring.byte", 0
    msg_fail        db "could not write to file: ", 0
    ; puts always appends a newline. Using printf with a "%s" format string may facilitate a non-newline solution
    bin_msg_fail    db "could not write to file: random_bitstring.bin", 0
    hex_msg_fail    db "could not write to file: random_bitstring.byte", 0

    hex_digits   db "0123456789abcdef"

section .bss
    timespec     resq 2
    bin_buffer   resb BIN_SIZE
    hex_buffer   resb HEX_SIZE

section .text
main:
    push rbp
    mov  rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    ; Six pushes after entry leave the stack 16-byte aligned for calls.

    lea  rdi, [start_msg_0]  ; a newline first
    call puts
    lea  rdi, [start_msg]
    call puts

    ; clock_gettime(CLOCK_MONOTONIC, &timespec)
    mov  edi, 1
    lea  rsi, [timespec]
    call clock_gettime
    ; test eax, eax
    ; js   .error

    ; seed = (uint32_t)((tv_sec * 13) ^ tv_nsec)
    ; tv_sec and tv_nsec are not overlapping
    ; I took this simple hashing idea from the Oxford Oberon-2 Compiler:
    mov  rax, [timespec]
    imul rax, rax, 13
    xor  rax, [timespec + 8]

    ; x[0] = raw_seed % 65519 + 1: scale the seed to range [1..m-1] for a safe 16 bit integer random seed
    mov  ecx, SEED_MOD
    xor  edx, edx
    div  rcx
    lea  r12d, [rdx + 1]          ; previous value = x[0]

    lea  r13, [bin_buffer]        ; binary output pointer
    lea  r14, [hex_buffer]        ; hex output pointer
    lea  r15, [hex_digits]
    mov  ebx, VALUE_COUNT

.generate:
    ; x[i] = (a * x[i-1] + c) % m
    mov  eax, r12d
    imul eax, eax, MULTIPLIER_a
    add  eax, INCREMENT_c
    xor  edx, edx
    mov  ecx, MODULUS_m
    div  ecx
    mov  r12d, edx                ; current x[i]

    ; Emit 16 binary digits, most significant bit first.
    mov  ecx, 15
.bin_loop:
    mov  eax, r12d
    shr  eax, cl
    and  eax, 1
    add  al, '0'
    mov  [r13], al
    inc  r13
    dec  ecx
    jns  .bin_loop

    ; Emit 4 lowercase hexadecimal digits, most significant first.
    mov  ecx, 12
.hex_loop:
    mov  eax, r12d
    shr  eax, cl
    and  eax, 15
    mov  al, [r15 + rax]
    mov  [r14], al
    inc  r14
    sub  ecx, 4
    jns  .hex_loop

    dec  ebx
    jnz  .generate

    xor  ebx, ebx                  ; overall return status: 0 = success

    ; Open and write the binary file.
    lea  rdi, [file_bits_x]
    lea  rsi, [file_mode]
    call fopen
    test rax, rax
    jz   .bin_open_error
    mov  r12, rax

    mov  rdi, bin_buffer
    mov  esi, 1
    mov  edx, BIN_SIZE
    mov  rcx, r12
    call fwrite
    cmp  rax, BIN_SIZE
    jne  .bin_write_error

    mov  rdi, r12
    call fclose
    test eax, eax
    jnz  .bin_close_error

    lea  rdi, [bin_msg_ok]
    call puts
    jmp  .write_hex

.bin_open_error:
    lea  rdi, [bin_msg_fail]
    call perror
    mov  ebx, 1
    jmp  .write_hex

.bin_write_error:
    lea  rdi, [bin_msg_fail]
    call perror
    mov  ebx, 1
    mov  rdi, r12
    call fclose
    jmp  .write_hex

.bin_close_error:
    lea  rdi, [bin_msg_fail]
    call perror
    mov  ebx, 1

.write_hex:
    ; Attempt the hexadecimal file regardless of the binary-file result.
    lea  rdi, [file_bits_hex]
    lea  rsi, [file_mode]
    call fopen
    test rax, rax
    jz   .hex_open_error
    mov  r12, rax

    mov  rdi, hex_buffer
    mov  esi, 1
    mov  edx, HEX_SIZE
    mov  rcx, r12
    call fwrite
    cmp  rax, HEX_SIZE
    jne  .hex_write_error

    mov  rdi, r12
    call fclose
    test eax, eax
    jnz  .hex_close_error

    lea  rdi, [hex_msg_ok]
    call puts
    jmp  .finish

.hex_open_error:
    lea  rdi, [hex_msg_fail]
    call perror
    mov  ebx, 1
    jmp  .finish

.hex_write_error:
    lea  rdi, [hex_msg_fail]
    call perror
    mov  ebx, 1
    mov  rdi, r12
    call fclose
    jmp  .finish

.hex_close_error:
    lea  rdi, [hex_msg_fail]
    call perror
    mov  ebx, 1

.finish:
    mov  eax, ebx                  ; return 1 if either file operation failed
    jmp  .done

; .error:
;     ; Print a system error message and return 1 in case that call clock_gettime failed.
;     xor  edi, edi
;     call perror
;     mov  eax, 1

.done:
    pop  r15
    pop  r14
    pop  r13
    pop  r12
    pop  rbx
    pop  rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits

; end of random_streams_for_perf_stats_x86-64-v1.asm
