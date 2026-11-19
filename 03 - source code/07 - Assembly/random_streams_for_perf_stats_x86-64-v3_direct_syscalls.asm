; random_streams_for_perf_stats_x86-64-v3_direct_syscalls.asm
;
; 2026-11-19
;
; Assembly code for NASM (Netwide Assembler) for x86-64 Linux, System V ABI, systems.
; It uses instructions of the x86-64-v3 feature level.
;
;
; build on Ubuntu 24 LTS: $ nasm -f elf64 random_streams_for_perf_stats_x86-64-v3_direct_syscalls.asm -o random_streams_for_perf_stats_x86-64-v3_direct_syscalls.o
;                         $ ld random_streams_for_perf_stats_x86-64-v3_direct_syscalls.o -o random_streams_for_perf_stats_x86-64-v3_direct_syscalls
;
; run on Ubuntu 24 LTS:   $ sudo sh -c 'echo 0 > /proc/sys/kernel/nmi_watchdog'  # disable the NMI watchdog to count all events
;                         $ sudo perf stat -r 20 ./random_streams_for_perf_stats_x86-64-v3_direct_syscalls => 0.0015...0.0024 <<<<<<<<
;                         $ sudo sh -c 'echo 1 > /proc/sys/kernel/nmi_watchdog'  # enable the NMI watchdog again
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
; transpiled from random_streams_for_perf_stats_x86-64-v2.asm with Google AI.


default rel

global _start

; --- Program Constants ---
%define END          62501  ; 62501 for exactly 1M binary digits (62500 values)
%define VALUE_COUNT  (END - 1)

%define BIN_SIZE     (VALUE_COUNT * 16)
%define HEX_SIZE     (VALUE_COUNT * 4)

%define MODULUS_m    65521  ; = 2^16 - 15
%define MULTIPLIER_a 17364
%define INCREMENT_C  0

; Magic constant for fast unsigned division by 65521:
; ceil(2^47 / 65521) = 2147975281 (0x80078071)
%define MAGIC_M      2147975281
%define MAGIC_SHIFT  47

%define SEED_MOD     (MODULUS_m - 2)

; --- Linux x86-64 Syscall Numbers & Flags ---
%define SYS_WRITE         1
%define SYS_CLOSE         3
%define SYS_EXIT          60
%define SYS_CLOCK_GETTIME 228
%define SYS_OPENAT        257

%define STDOUT_FD         1
%define CLOCK_MONOTONIC   1
%define AT_FDCWD          -100
%define O_WRONLY          1
%define O_CREAT           64
%define O_TRUNC           512
%define OPEN_FLAGS        (O_WRONLY | O_CREAT | O_TRUNC) ; 0x241 (577)
%define FILE_PERMS        0644o                          ; rw-r--r--

section .rodata
    align 32
    bin_shuf_mask_256:
        db 1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,0
        db 3,3,3,3,3,3,3,3, 2,2,2,2,2,2,2,2

    bin_bit_mask_256:
        db 128,64,32,16,8,4,2,1, 128,64,32,16,8,4,2,1
        db 128,64,32,16,8,4,2,1, 128,64,32,16,8,4,2,1

    ascii_zeros_256:
        times 32 db '0'

    align 16
    hex_shuf_mask_128:
        db 3, 1, 2, 0,  7, 5, 6, 4,  11, 9, 10, 8,  15, 13, 14, 12

    hex_ascii_lut:
        db "0123456789abcdef"

    file_bits_x     db "random_bitstring.bin", 0
    file_bits_hex   db "random_bitstring.byte", 0

    start_msg       db 10, "generating a random bit stream...", 10
    start_msg_len   equ $ - start_msg

    bin_msg_ok      db "Bit stream has been written to disk under name:  random_bitstring.bin", 10
    bin_msg_ok_len  equ $ - bin_msg_ok

    hex_msg_ok      db "Byte stream has been written to disk under name: random_bitstring.byte", 10
    hex_msg_ok_len  equ $ - hex_msg_ok

    bin_msg_fail    db "could not write to file: random_bitstring.bin", 10
    bin_msg_fail_len equ $ - bin_msg_fail

    hex_msg_fail    db "could not write to file: random_bitstring.byte", 10
    hex_msg_fail_len equ $ - hex_msg_fail

section .bss align=32
    timespec     resq 2
    alignb 32
    bin_buffer   resb BIN_SIZE
    alignb 32
    hex_buffer   resb HEX_SIZE

section .text
_start:
    ; Print initial newline and status message in a single syscall
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
    mov  rax, [timespec]
    imul rax, rax, 13
    xor  rax, [timespec + 8]

    ; x = raw_seed % 65519 + 1
    mov  ecx, SEED_MOD
    xor  edx, edx
    div  rcx
    lea  r12d, [rdx + 1]          ; r12d = initial state x

    lea  r13, [bin_buffer]        ; 32-byte aligned binary output pointer
    lea  r14, [hex_buffer]        ; 32-byte aligned hex output pointer
    mov  ebx, VALUE_COUNT / 4     ; 62500 / 4 = 15625 iterations

    ; Load 256-bit AVX2 constants
    vmovdqa ymm8,  [bin_shuf_mask_256]
    vmovdqa ymm9,  [bin_bit_mask_256]
    vmovdqa ymm10, [ascii_zeros_256]

    ; Load 128-bit AVX2 constants for hex conversion
    vmovdqa xmm11, [hex_ascii_lut]
    vmovdqa xmm12, [hex_shuf_mask_128]
    mov     eax,   0x0F0F0F0F
    vmovd   xmm13, eax
    vpbroadcastd xmm13, xmm13

    mov     r9d,   MODULUS_m
    mov     r10,   MAGIC_M
    mov     r15,   MAGIC_SHIFT

align 32
.generate_4x:
    ; --- Compute x0 = x[i] ---
    imul eax, r12d, MULTIPLIER_a
    add  eax, INCREMENT_C
    mov  rcx, rax
    imul rcx, r10
    shrx rcx, rcx, r15
    imul ecx, r9d
    sub  eax, ecx
    mov  r8d, eax                 ; r8d = x0

    ; --- Compute x1 = x[i+1] ---
    imul eax, r8d, MULTIPLIER_a
    add  eax, INCREMENT_C
    mov  rcx, rax
    imul rcx, r10
    shrx rcx, rcx, r15
    imul ecx, r9d
    sub  eax, ecx
    mov  r11d, eax                ; r11d = x1

    ; --- Compute x2 = x[i+2] ---
    imul eax, r11d, MULTIPLIER_a
    add  eax, INCREMENT_C
    mov  rcx, rax
    imul rcx, r10
    shrx rcx, rcx, r15
    imul ecx, r9d
    sub  eax, ecx
    mov  edx, eax                 ; edx = x2

    ; --- Compute x3 = x[i+3] ---
    imul eax, edx, MULTIPLIER_a
    add  eax, INCREMENT_C
    mov  rcx, rax
    imul rcx, r10
    shrx rcx, rcx, r15
    imul ecx, r9d
    sub  eax, ecx
    mov  r12d, eax                ; r12d = x3

    ; --- Pack (x0, x1) into esi and (x2, x3) into eax ---
    mov  esi, r11d
    shl  esi, 16
    or   esi, r8d                 ; esi = [x1 (high 16), x0 (low 16)]

    mov  eax, r12d
    shl  eax, 16
    or   eax, edx                 ; eax = [x3 (high 16), x2 (low 16)]

    ; --- Convert x0 & x1 to 32 binary ASCII chars (AVX2 256-bit) ---
    vmovd        xmm0, esi
    vpbroadcastd ymm0, xmm0
    vpshufb      ymm0, ymm0, ymm8
    vpand        ymm0, ymm0, ymm9
    vpcmpeqb     ymm0, ymm0, ymm9
    vpsubb       ymm0, ymm10, ymm0
    vmovdqa      [r13], ymm0

    ; --- Convert x2 & x3 to 32 binary ASCII chars (AVX2 256-bit) ---
    vmovd        xmm1, eax
    vpbroadcastd ymm1, xmm1
    vpshufb      ymm1, ymm1, ymm8
    vpand        ymm1, ymm1, ymm9
    vpcmpeqb     ymm1, ymm1, ymm9
    vpsubb       ymm1, ymm10, ymm1
    vmovdqa      [r13 + 32], ymm1

    ; --- Convert x0, x1, x2, x3 to 16 hex ASCII chars (AVX2 128-bit) ---
    shl          rax, 32
    or           rax, rsi
    vmovq        xmm2, rax
    vpsrlw       xmm3, xmm2, 4
    vpand        xmm2, xmm2, xmm13
    vpand        xmm3, xmm3, xmm13
    vpunpcklbw   xmm2, xmm2, xmm3
    vpshufb      xmm2, xmm2, xmm12
    vpshufb      xmm2, xmm11, xmm2
    vmovdqa      [r14], xmm2

    add  r13, 64
    add  r14, 16
    dec  ebx
    jnz  .generate_4x

    ; =========================================================================
    ; Direct Syscall File I/O
    ; =========================================================================
    xor  ebx, ebx                 ; ebx = overall exit status (0 = success)

    ; --- 1. Write random_bitstring.bin ---
    mov  eax, SYS_OPENAT
    mov  edi, AT_FDCWD
    lea  rsi, [file_bits_x]
    mov  edx, OPEN_FLAGS
    mov  r10d, FILE_PERMS
    syscall
    test rax, rax
    js   .bin_open_error          ; negative return value indicates -errno
    mov  r12, rax                 ; r12 = fd

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
    mov  edi, STDOUT_FD
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
    mov  edi, STDOUT_FD
    lea  rsi, [bin_msg_fail]
    mov  edx, bin_msg_fail_len
    syscall
    jmp  .write_hex

.bin_close_error:
    mov  ebx, 1
    mov  eax, SYS_WRITE
    mov  edi, STDOUT_FD
    lea  rsi, [bin_msg_fail]
    mov  edx, bin_msg_fail_len
    syscall

    ; --- 2. Write random_bitstring.byte ---
.write_hex:
    mov  eax, SYS_OPENAT
    mov  edi, AT_FDCWD
    lea  rsi, [file_bits_hex]
    mov  edx, OPEN_FLAGS
    mov  r10d, FILE_PERMS
    syscall
    test rax, rax
    js   .hex_open_error
    mov  r12, rax                 ; r12 = fd

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
    mov  edi, STDOUT_FD
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
    mov  edi, STDOUT_FD
    lea  rsi, [hex_msg_fail]
    mov  edx, hex_msg_fail_len
    syscall
    jmp  .exit

.hex_close_error:
    mov  ebx, 1
    mov  eax, SYS_WRITE
    mov  edi, STDOUT_FD
    lea  rsi, [hex_msg_fail]
    mov  edx, hex_msg_fail_len
    syscall

.exit:
    mov  eax, SYS_EXIT
    mov  edi, ebx
    syscall

section .note.GNU-stack noalloc noexec nowrite progbits

; end of random_streams_for_perf_stats_x86-64-v3_direct_syscalls.asm
