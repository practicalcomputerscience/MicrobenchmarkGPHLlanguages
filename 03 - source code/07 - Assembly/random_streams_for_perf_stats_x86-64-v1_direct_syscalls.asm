; random_streams_for_perf_stats_x86-64-v1_direct_syscalls.asm
;
; 2026-11-19
;
; Assembly code for NASM (Netwide Assembler) for x86-64 Linux, System V ABI, systems.
; It uses only baseline x86-64 instructions.
;
;
; build on Ubuntu 24 LTS: $ nasm -f elf64 random_streams_for_perf_stats_x86-64-v1_direct_syscalls.asm -o random_streams_for_perf_stats_x86-64-v1_direct_syscalls.o
;                         $ ld random_streams_for_perf_stats_x86-64-v1_direct_syscalls.o -o random_streams_for_perf_stats_x86-64-v1_direct_syscalls
;
; run on Ubuntu 24 LTS:   $ sudo perf stat -r 20 ./random_streams_for_perf_stats_x86-64-v1_direct_syscalls
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
; transpiled from transpiled from random_streams_for_perf_stats_x86-64-v1.asm with Google AI.


default rel

global _start

; --- Program Constants ---
%define END          62501  ; 62501 for exactly 1M binary digits
%define VALUE_COUNT  (END - 1)

%define BIN_SIZE     (VALUE_COUNT * 16)
%define HEX_SIZE     (VALUE_COUNT * 4)

%define MODULUS_m    65521  ; = 2^16 - 15
%define MULTIPLIER_a 17364
%define INCREMENT_c  0

%define SEED_MOD     (MODULUS_m - 2)

; --- Linux x86-64 Syscall Numbers & Flags ---
%define SYS_WRITE         1
%define SYS_CLOSE         3
%define SYS_EXIT          60
%define SYS_CLOCK_GETTIME 228
%define SYS_OPENAT        257

%define STDOUT_FD         1
%define STDERR_FD         2
%define CLOCK_MONOTONIC   1
%define AT_FDCWD          -100
%define O_WRONLY          1
%define O_CREAT           64
%define O_TRUNC           512
%define OPEN_FLAGS        (O_WRONLY | O_CREAT | O_TRUNC) ; 0x241 (577)
%define FILE_PERMS        0644o                          ; rw-r--r--

section .rodata
    file_bits_x      db "random_bitstring.bin", 0
    file_bits_hex    db "random_bitstring.byte", 0

    start_msg        db 10, "generating a random bit stream...", 10
    start_msg_len    equ $ - start_msg

    bin_msg_ok       db "Bit stream has been written to disk under name:  random_bitstring.bin", 10
    bin_msg_ok_len   equ $ - bin_msg_ok

    hex_msg_ok       db "Byte stream has been written to disk under name: random_bitstring.byte", 10
    hex_msg_ok_len   equ $ - hex_msg_ok

    bin_msg_fail     db "could not write to file: random_bitstring.bin", 10
    bin_msg_fail_len equ $ - bin_msg_fail

    hex_msg_fail     db "could not write to file: random_bitstring.byte", 10
    hex_msg_fail_len equ $ - hex_msg_fail

    hex_digits       db "0123456789abcdef"

section .bss
    timespec     resq 2
    bin_buffer   resb BIN_SIZE
    hex_buffer   resb HEX_SIZE

section .text
_start:
    ; Print initial newline and start message
    mov  eax, SYS_WRITE
    mov  edi, STDOUT_FD
    lea  rsi, [start_msg]
    mov  edx, start_msg_len
    syscall

    ; clock_gettime(CLOCK_MONOTONIC, &timespec)
    mov  eax, SYS_CLOCK_GETTIME
    mov  edi, CLOCK_MONOTONIC
    lea  rsi, [timespec]
    syscall

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

    xor  ebx, ebx                 ; overall exit status: 0 = success

    ; =========================================================================
    ; Open and write the binary file (random_bitstring.bin)
    ; =========================================================================
    mov  eax, SYS_OPENAT
    mov  edi, AT_FDCWD
    lea  rsi, [file_bits_x]
    mov  edx, OPEN_FLAGS
    mov  r10d, FILE_PERMS
    syscall
    test rax, rax
    js   .bin_open_error
    mov  r12, rax                 ; save file descriptor

    lea  rsi, [bin_buffer]
    mov  edx, BIN_SIZE
.bin_write_loop:
    mov  eax, SYS_WRITE
    mov  edi, r12d
    syscall
    test rax, rax
    jle  .bin_write_error
    add  rsi, rax
    sub  rdx, rax
    jnz  .bin_write_loop

    mov  eax, SYS_CLOSE
    mov  edi, r12d
    syscall
    test rax, rax
    js   .bin_close_error

    mov  eax, SYS_WRITE
    mov  edi, STDOUT_FD
    lea  rsi, [bin_msg_ok]
    mov  edx, bin_msg_ok_len
    syscall
    jmp  .write_hex

.bin_open_error:
    mov  ebx, 1
    mov  eax, SYS_WRITE
    mov  edi, STDERR_FD
    lea  rsi, [bin_msg_fail]
    mov  edx, bin_msg_fail_len
    syscall
    jmp  .write_hex

.bin_write_error:
    mov  ebx, 1
    mov  eax, SYS_CLOSE
    mov  edi, r12d
    syscall
    mov  eax, SYS_WRITE
    mov  edi, STDERR_FD
    lea  rsi, [bin_msg_fail]
    mov  edx, bin_msg_fail_len
    syscall
    jmp  .write_hex

.bin_close_error:
    mov  ebx, 1
    mov  eax, SYS_WRITE
    mov  edi, STDERR_FD
    lea  rsi, [bin_msg_fail]
    mov  edx, bin_msg_fail_len
    syscall

    ; =========================================================================
    ; Open and write the hexadecimal file (random_bitstring.byte)
    ; =========================================================================
.write_hex:
    mov  eax, SYS_OPENAT
    mov  edi, AT_FDCWD
    lea  rsi, [file_bits_hex]
    mov  edx, OPEN_FLAGS
    mov  r10d, FILE_PERMS
    syscall
    test rax, rax
    js   .hex_open_error
    mov  r12, rax                 ; save file descriptor

    lea  rsi, [hex_buffer]
    mov  edx, HEX_SIZE
.hex_write_loop:
    mov  eax, SYS_WRITE
    mov  edi, r12d
    syscall
    test rax, rax
    jle  .hex_write_error
    add  rsi, rax
    sub  rdx, rax
    jnz  .hex_write_loop

    mov  eax, SYS_CLOSE
    mov  edi, r12d
    syscall
    test rax, rax
    js   .hex_close_error

    mov  eax, SYS_WRITE
    mov  edi, STDOUT_FD
    lea  rsi, [hex_msg_ok]
    mov  edx, hex_msg_ok_len
    syscall
    jmp  .exit

.hex_open_error:
    mov  ebx, 1
    mov  eax, SYS_WRITE
    mov  edi, STDERR_FD
    lea  rsi, [hex_msg_fail]
    mov  edx, hex_msg_fail_len
    syscall
    jmp  .exit

.hex_write_error:
    mov  ebx, 1
    mov  eax, SYS_CLOSE
    mov  edi, r12d
    syscall
    mov  eax, SYS_WRITE
    mov  edi, STDERR_FD
    lea  rsi, [hex_msg_fail]
    mov  edx, hex_msg_fail_len
    syscall
    jmp  .exit

.hex_close_error:
    mov  ebx, 1
    mov  eax, SYS_WRITE
    mov  edi, STDERR_FD
    lea  rsi, [hex_msg_fail]
    mov  edx, hex_msg_fail_len
    syscall

.exit:
    mov  eax, SYS_EXIT
    mov  edi, ebx
    syscall

section .note.GNU-stack noalloc noexec nowrite progbits

; end of random_streams_for_perf_stats_x86-64-v1_direct_syscalls.asm
