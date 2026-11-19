; random_streams_for_perf_stats_x86-64-v3.asm
;
; 2026-11-19
;
; Assembly code for NASM (Netwide Assembler) for x86-64 Linux, System V ABI, systems.
; It uses instructions of the x86-64-v3 feature level.
;
;
; build on Ubuntu 24 LTS: $ nasm -f elf64 random_streams_for_perf_stats_x86-64-v3.asm -o random_streams_for_perf_stats_x86-64-v3.o
;                         $ gcc -no-pie random_streams_for_perf_stats_x86-64-v3.o -o random_streams_for_perf_stats_x86-64-v3
;
; run on Ubuntu 24 LTS:   $ sudo sh -c 'echo 0 > /proc/sys/kernel/nmi_watchdog'  # disable the NMI watchdog to count all events
;                         $ sudo perf stat -r 20 ./random_streams_for_perf_stats_x86-64-v3 => 0.0016...0.0024 <<<<<<<<
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

global main

extern clock_gettime
extern fopen
extern fwrite
extern fclose
extern puts
extern perror

%define END         62501  ; 62501 for exactly 1M binary digits (62500 values)
%define VALUE_COUNT (END - 1)

%define BIN_SIZE    (VALUE_COUNT * 16)
%define HEX_SIZE    (VALUE_COUNT * 4)

%define MODULUS_m    65521  ; = 2^16 - 15
%define MULTIPLIER_a 17364
%define INCREMENT_C  0

; Magic constant for fast unsigned division by 65521:
; ceil(2^47 / 65521) = 2147975281 (0x80078071)
%define MAGIC_M      2147975281
%define MAGIC_SHIFT  47

%define SEED_MOD     (MODULUS_m - 2)

section .rodata
    align 32
    ; AVX2 shuffle mask (operates per 128-bit lane):
    ; Lower 128-bit lane broadcasts bytes 1 (MSB) and 0 (LSB) of x[i]
    ; Upper 128-bit lane broadcasts bytes 3 (MSB) and 2 (LSB) of x[i+1]
    bin_shuf_mask_256:
        db 1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,0
        db 3,3,3,3,3,3,3,3, 2,2,2,2,2,2,2,2

    ; Bit masks for isolating bits 7 down to 0 across all 32 bytes
    bin_bit_mask_256:
        db 128,64,32,16,8,4,2,1, 128,64,32,16,8,4,2,1
        db 128,64,32,16,8,4,2,1, 128,64,32,16,8,4,2,1

    ascii_zeros_256:
        times 32 db '0'

    align 16
    ; Reorders 16 unpacked nibbles from 4 little-endian 16-bit words into MSB-first order
    hex_shuf_mask_128:
        db 3, 1, 2, 0,  7, 5, 6, 4,  11, 9, 10, 8,  15, 13, 14, 12

    hex_ascii_lut:
        db "0123456789abcdef"

    file_bits_x   db "random_bitstring.bin", 0
    file_bits_hex db "random_bitstring.byte", 0
    file_mode     db "wb", 0

    start_msg_0   db "", 0
    start_msg     db "generating a random bit stream...", 0

    bin_msg_ok    db "Bit stream has been written to disk under name:  random_bitstring.bin", 0
    hex_msg_ok    db "Byte stream has been written to disk under name: random_bitstring.byte", 0
    bin_msg_fail  db "could not write to file: random_bitstring.bin", 0
    hex_msg_fail  db "could not write to file: random_bitstring.byte", 0

section .bss align=32
    timespec     resq 2
    alignb 32
    bin_buffer   resb BIN_SIZE
    alignb 32
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

    lea  rdi, [start_msg_0]
    call puts
    lea  rdi, [start_msg]
    call puts

    ; clock_gettime(CLOCK_MONOTONIC, &timespec)
    mov  edi, 1
    lea  rsi, [timespec]
    call clock_gettime

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
    vpbroadcastd xmm13, xmm13     ; 16 bytes of 0x0F

    mov     r9d,   MODULUS_m
    mov     r10,   MAGIC_M
    mov     r15,   MAGIC_SHIFT    ; shift count for BMI2 shrx

align 32
.generate_4x:
    ; --- Compute x0 = x[i] ---
    imul eax, r12d, MULTIPLIER_a
    add  eax, INCREMENT_C
    mov  rcx, rax
    imul rcx, r10
    shrx rcx, rcx, r15            ; BMI2 non-flag-affecting shift right by 47
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
    mov  r12d, eax                ; r12d = x3 (also state for next iteration)

    ; --- Pack (x0, x1) into esi and (x2, x3) into edi ---
    mov  esi, r11d
    shl  esi, 16
    or   esi, r8d                 ; esi = [x1 (high 16), x0 (low 16)]

    mov  edi, r12d
    shl  edi, 16
    or   edi, edx                 ; edi = [x3 (high 16), x2 (low 16)]

    ; --- Convert x0 & x1 to 32 binary ASCII chars (AVX2 256-bit) ---
    vmovd        xmm0, esi
    vpbroadcastd ymm0, xmm0       ; broadcast [x0, x1] to all eight 32-bit lanes
    vpshufb      ymm0, ymm0, ymm8 ; lane 0 -> x0 bytes, lane 1 -> x1 bytes
    vpand        ymm0, ymm0, ymm9 ; isolate individual bits
    vpcmpeqb     ymm0, ymm0, ymm9 ; 0xFF (-1) where bit == 1, 0x00 where bit == 0
    vpsubb       ymm0, ymm10, ymm0; '0' - (-1) = '1', '0' - 0 = '0'
    vmovdqa      [r13], ymm0      ; 32-byte aligned store

    ; --- Convert x2 & x3 to 32 binary ASCII chars (AVX2 256-bit) ---
    vmovd        xmm1, edi
    vpbroadcastd ymm1, xmm1
    vpshufb      ymm1, ymm1, ymm8
    vpand        ymm1, ymm1, ymm9
    vpcmpeqb     ymm1, ymm1, ymm9
    vpsubb       ymm1, ymm10, ymm1
    vmovdqa      [r13 + 32], ymm1 ; 32-byte aligned store

    ; --- Convert x0, x1, x2, x3 to 16 hex ASCII chars (AVX2 128-bit) ---
    ; Combine esi (x0, x1) and edi (x2, x3) into a single 64-bit register
    shl          rdi, 32
    or           rdi, rsi         ; rdi = [x3, x2, x1, x0]
    vmovq        xmm2, rdi
    vpsrlw       xmm3, xmm2, 4    ; high nibbles shifted down
    vpand        xmm2, xmm2, xmm13; mask low nibbles
    vpand        xmm3, xmm3, xmm13; mask high nibbles
    vpunpcklbw   xmm2, xmm2, xmm3 ; interleave all 16 nibbles
    vpshufb      xmm2, xmm2, xmm12; reorder to MSB-first per 16-bit word
    vpshufb      xmm2, xmm11, xmm2; translate all 16 nibbles to ASCII in 1 cycle!
    vmovdqa      [r14], xmm2      ; 16-byte aligned store

    add  r13, 64
    add  r14, 16
    dec  ebx
    jnz  .generate_4x

    ; Clean up upper YMM state before calling external glibc functions
    vzeroupper

    ; --- File I/O (Unchanged) ---
    xor  ebx, ebx

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
    mov  eax, ebx
    pop  r15
    pop  r14
    pop  r13
    pop  r12
    pop  rbx
    pop  rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits

; end of random_streams_for_perf_stats_x86-64-v3.asm
