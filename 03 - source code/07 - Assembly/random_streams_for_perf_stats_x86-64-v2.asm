; random_streams_for_perf_stats_x86-64-v2.asm
;
; 2026-11-19
;
; Assembly code for NASM (Netwide Assembler) for x86-64 Linux, System V ABI, systems.
; It uses instructions of the x86-64-v2 feature level.
;
;
; build on Ubuntu 24 LTS: $ nasm -f elf64 random_streams_for_perf_stats_x86-64-v2.asm -o random_streams_for_perf_stats_x86-64-v2.o
;                         $ gcc -no-pie random_streams_for_perf_stats_x86-64-v2.o -o random_streams_for_perf_stats_x86-64-v2
;
; run on Ubuntu 24 LTS:   $ sudo sh -c 'echo 0 > /proc/sys/kernel/nmi_watchdog'  # disable the NMI watchdog to count all events
;                         $ sudo perf stat -r 20 ./random_streams_for_perf_stats_x86-64-v2 => 0.0016...0.0022 <<<<<<<<
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
; transpiled from random_streams_for_perf_stats_x86-64-v1.asm with Google AI.


default rel

global main

extern clock_gettime
extern fopen
extern fwrite
extern fclose
extern puts
extern perror

%define END         62501  ; 62501 for exactly 1M binary digits (62500 values)
%define VALUE_COUNT (END - 1)  ; VALUE_COUNT is an even number here, so no cleanup tail loop is required.
                               ; if VALUE_COUNT is odd, add a single-element tail handler.

%define BIN_SIZE    (VALUE_COUNT * 16)
%define HEX_SIZE    (VALUE_COUNT * 4)

%define MODULUS_m    65521  ; = 2^16 - 15
%define MULTIPLIER_a 17364
%define INCREMENT_c  0

; Magic constant for fast unsigned division by 65521:
; ceil(2^47 / 65521) = 2147975281 (0x80078071)
%define MAGIC_M      2147975281
%define MAGIC_SHIFT  47

%define SEED_MOD     (MODULUS_m - 2)

section .rodata
    align 16
    ; SSSE3 shuffle mask: broadcasts low byte (bits 0..7) to bytes 8..15,
    ; and high byte (bits 8..15) to bytes 0..7 (MSB first order)
    bin_shuf_mask: db 1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,0

    ; Bit masks for isolating bits 7 down to 0 in each byte
    bin_bit_mask:  db 128,64,32,16,8,4,2,1, 128,64,32,16,8,4,2,1

    ; ASCII constants for '0' (0x30) and '1' (0x31)
    ascii_zeros:   times 16 db '0'
    ascii_ones:    times 16 db '1'

    ; SSSE3 shuffle mask to extract 4 nibbles (MSB first) from a 16-bit word
    ; Input word in low 16 bits: [byte0 (low), byte1 (high)]
    ; We isolate low/high nibbles via pand/psrlw first, then pack via pshufb
    hex_shuf_mask: db 3, 1, 2, 0,  7, 5, 6, 4,  0x80,0x80,0x80,0x80, 0x80,0x80,0x80,0x80
    hex_ascii_lut: db "0123456789abcdef"

    file_bits_x   db "random_bitstring.bin", 0
    file_bits_hex db "random_bitstring.byte", 0
    file_mode     db "wb", 0

    start_msg_0   db "", 0
    start_msg     db "generating a random bit stream...", 0

    bin_msg_ok    db "Bit stream has been written to disk under name:  random_bitstring.bin", 0
    hex_msg_ok    db "Byte stream has been written to disk under name: random_bitstring.byte", 0
    bin_msg_fail  db "could not write to file: random_bitstring.bin", 0
    hex_msg_fail  db "could not write to file: random_bitstring.byte", 0

section .bss
    timespec     resq 2
    align 16
    bin_buffer   resb BIN_SIZE
    align 16
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

    ; x[0] = raw_seed % 65519 + 1
    mov  ecx, SEED_MOD
    xor  edx, edx
    div  rcx
    lea  r12d, [rdx + 1]          ; r12d = x[0]

    lea  r13, [bin_buffer]        ; binary output pointer (16-byte aligned)
    lea  r14, [hex_buffer]        ; hex output pointer (16-byte aligned)
    mov  ebx, VALUE_COUNT / 2     ; process 2 values (32 bin chars, 8 hex chars) per iteration

    ; Load SIMD constants into XMM registers (x86-64-v2 / SSE2+SSSE3)
    movdqa xmm8,  [bin_shuf_mask]
    movdqa xmm9,  [bin_bit_mask]
    movdqa xmm10, [ascii_zeros]
    movdqa xmm11, [ascii_ones]
    movdqa xmm12, [hex_ascii_lut]
    movdqa xmm13, [hex_shuf_mask]
    mov    r8d,   0x0F0F0F0F
    movd   xmm14, r8d             ; nibble mask 0x0F0F0F0F

    mov    r9d,   MODULUS_m
    mov    r10,   MAGIC_M

align 16
.generate_2x:
    ; --- Step 1: Compute x[i] using fast magic-constant modulo ---
    imul eax, r12d, MULTIPLIER_a
    add  eax, INCREMENT_c         ; eax = dividend (max ~1.13e9 < 2^31)
    mov  rcx, rax
    imul rcx, r10                 ; rcx = dividend * MAGIC_M
    shr  rcx, MAGIC_SHIFT         ; rcx = quotient = dividend / 65521
    imul ecx, r9d                 ; ecx = quotient * 65521
    sub  eax, ecx                 ; eax = remainder = x[i]
    mov  r11d, eax                ; save x[i] in r11d

    ; --- Step 2: Compute x[i+1] ---
    imul eax, r11d, MULTIPLIER_a
    add  eax, INCREMENT_c
    mov  rcx, rax
    imul rcx, r10
    shr  rcx, MAGIC_SHIFT
    imul ecx, r9d
    sub  eax, ecx                 ; eax = x[i+1]
    mov  r12d, eax                ; update state for next loop iteration

    ; --- Step 3: Convert x[i] (r11d) to 16 binary ASCII chars (SSSE3) ---
    movd    xmm0, r11d
    pshufb  xmm0, xmm8            ; replicate high byte to [0..7], low byte to [8..15]
    pand    xmm0, xmm9            ; isolate individual bits
    pcmpeqb xmm0, xmm9            ; 0xFF where bit == 1, 0x00 where bit == 0
    movdqa  xmm1, xmm0
    pand    xmm0, xmm11           ; select '1' where bit == 1
    pandn   xmm1, xmm10           ; select '0' where bit == 0
    por     xmm0, xmm1            ; combine into 16 ASCII bytes
    movdqa  [r13], xmm0           ; aligned 16-byte store

    ; --- Step 4: Convert x[i+1] (r12d) to 16 binary ASCII chars (SSSE3) ---
    movd    xmm2, r12d
    pshufb  xmm2, xmm8
    pand    xmm2, xmm9
    pcmpeqb xmm2, xmm9
    movdqa  xmm3, xmm2
    pand    xmm2, xmm11
    pandn   xmm3, xmm10
    por     xmm2, xmm3
    movdqa  [r13 + 16], xmm2      ; aligned 16-byte store

    ; --- Step 5: Convert both x[i] and x[i+1] to 8 hex ASCII chars (SSSE3) ---
    ; Pack x[i] (low 16 bits) and x[i+1] (bits 16..31) into one 32-bit register
    mov     eax, r12d
    shl     eax, 16
    or      eax, r11d             ; eax = [x[i+1] (high 16), x[i] (low 16)]

    movd    xmm4, eax             ; bytes: [x0_lo, x0_hi, x1_lo, x1_hi]
    movdqa  xmm5, xmm4
    psrlw   xmm5, 4               ; shift right by 4 to get high nibbles in low positions
    pand    xmm4, xmm14           ; xmm4 = low nibbles:  [n0, n2, n4, n6]
    pand    xmm5, xmm14           ; xmm5 = high nibbles: [n1, n3, n5, n7]
    punpcklbw xmm4, xmm5          ; interleave: [n0, n1, n2, n3, n4, n5, n6, n7]
    pshufb  xmm4, xmm13           ; reorder to MSB-first per 16-bit word
    movdqa  xmm5, xmm12           ; copy hex ASCII LUT ("0123456789abcdef")
    pshufb  xmm5, xmm4            ; parallel 8-byte LUT lookup!
    movq    [r14], xmm5           ; store all 8 hex characters at once

    add  r13, 32
    add  r14, 8
    dec  ebx
    jnz  .generate_2x

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

; end of random_streams_for_perf_stats_x86-64-v2.asm
