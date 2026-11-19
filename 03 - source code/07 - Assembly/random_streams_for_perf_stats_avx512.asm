; random_streams_for_perf_stats_avx512.asm
;
; 2026-11-19
;
; Assembly code for NASM (Netwide Assembler) for x86-64 Linux, System V ABI, systems.
; It uses instructions of the x86-64-v2 feature level.
;
;
; build on Ubuntu 24 LTS: $ nasm -f elf64 random_streams_for_perf_stats_avx512.asm -o random_streams_for_perf_stats_avx512.o
;                         $ gcc -no-pie random_streams_for_perf_stats_avx512.o -o random_streams_for_perf_stats_avx512
;
; run on Ubuntu 24 LTS:   $ sudo sh -c 'echo 0 > /proc/sys/kernel/nmi_watchdog'  # disable the NMI watchdog to count all events
;                         $ sudo perf stat -r 20 ./random_streams_for_perf_stats_avx512 => 0.0016...0.0026 <<<<<<<<
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
%define VALUE_COUNT (END - 1)  ; 62500 is evenly divisible by 4 (no tail loop needed)

%define BIN_SIZE    (VALUE_COUNT * 16)
%define HEX_SIZE    (VALUE_COUNT * 4)

%define MODULUS_m     65521  ; = 2^16 - 15
%define MULTIPLIER_a  17364
; Jump-ahead multiplier for 4 steps: (17364^4) mod 65521 = 15251
%define MULTIPLIER_a4 15251
%define INCREMENT_c   0

; Magic constant for fast unsigned division by 65521:
; ceil(2^47 / 65521) = 2147975281 (0x80078071)
%define MAGIC_M       2147975281
%define MAGIC_SHIFT   47

%define SEED_MOD      (MODULUS_m - 2)

section .rodata
    align 64
    ascii_zeros_512: times 64 db '0'
    ascii_ones_512:  times 64 db '1'

    align 16
    ; Reorders 16 unpacked nibbles from 4 little-endian 16-bit words into MSB-first order
    hex_shuf_mask:   db 3, 1, 2, 0,  7, 5, 6, 4,  11, 9, 10, 8,  15, 13, 14, 12
    hex_ascii_lut:   db "0123456789abcdef"

    file_bits_x      db "random_bitstring.bin", 0
    file_bits_hex    db "random_bitstring.byte", 0
    file_mode        db "wb", 0

    start_msg_0      db "", 0
    start_msg        db "generating a random bit stream...", 0

    bin_msg_ok       db "Bit stream has been written to disk under name:  random_bitstring.bin", 0
    hex_msg_ok       db "Byte stream has been written to disk under name: random_bitstring.byte", 0
    bin_msg_fail     db "could not write to file: random_bitstring.bin", 0
    hex_msg_fail     db "could not write to file: random_bitstring.byte", 0

section .bss align=64
    timespec     resq 2
    alignb 64
    bin_buffer   resb BIN_SIZE
    alignb 64
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
    lea  r8d, [rdx + 1]           ; r8d = x[0]

    ; Precompute x[1], x[2], x[3] so all 4 LCG lanes run in parallel
    mov  r9d, MODULUS_m
    mov  r10, MAGIC_M

    ; x[1] = (a * x[0]) % m
    imul eax, r8d, MULTIPLIER_a
    mov  rcx, rax
    imul rcx, r10
    shr  rcx, MAGIC_SHIFT
    imul ecx, r9d
    sub  eax, ecx
    mov  r11d, eax                ; r11d = x[1]

    ; x[2] = (a * x[1]) % m
    imul eax, r11d, MULTIPLIER_a
    mov  rcx, rax
    imul rcx, r10
    shr  rcx, MAGIC_SHIFT
    imul ecx, r9d
    sub  eax, ecx
    mov  r12d, eax                ; r12d = x[2]

    ; x[3] = (a * x[2]) % m
    imul eax, r12d, MULTIPLIER_a
    mov  rcx, rax
    imul rcx, r10
    shr  rcx, MAGIC_SHIFT
    imul ecx, r9d
    sub  eax, ecx
    mov  r15d, eax                ; r15d = x[3]

    lea  r13, [bin_buffer]        ; 64-byte aligned binary output pointer
    lea  r14, [hex_buffer]        ; 64-byte aligned hex output pointer
    mov  ebx, VALUE_COUNT / 4     ; 62500 / 4 = 15625 iterations

    ; Load AVX-512 & AVX2 constants
    vmovdqa64 zmm10, [ascii_zeros_512]
    vmovdqa64 zmm11, [ascii_ones_512]

    vmovdqa   xmm12, [hex_ascii_lut]
    vmovdqa   xmm13, [hex_shuf_mask]
    mov       eax,   0x0F0F0F0F
    vmovd     xmm14, eax
    vpbroadcastd xmm14, xmm14     ; 16 bytes of 0x0F

align 32
.generate_4x_avx512:
    ; --- Step 1: Pack current [x0, x1, x2, x3] for Hex and Binary conversion ---
    ; Pack [x1 (high 16), x0 (low 16)] into esi
    mov  esi, r11d
    shl  esi, 16
    or   esi, r8d

    ; Pack [x3 (high 16), x2 (low 16)] into edi
    mov  edi, r15d
    shl  edi, 16
    or   edi, r12d

    ; Combine into 64-bit rax = [x3, x2, x1, x0] (little-endian word order)
    mov  rax, rdi
    shl  rax, 32
    or   rax, rsi

    ; --- Step 2: Convert all 4 values to 16 hex ASCII chars (AVX2 128-bit) ---
    vmovq      xmm2, rax
    vpsrlw     xmm3, xmm2, 4
    vpand      xmm2, xmm2, xmm14
    vpand      xmm3, xmm3, xmm14
    vpunpcklbw xmm2, xmm2, xmm3
    vpshufb    xmm2, xmm2, xmm13
    vpshufb    xmm2, xmm12, xmm2
    vmovdqa    [r14], xmm2        ; 16-byte aligned store

    ; --- Step 3: Convert all 64 bits to 64 binary ASCII chars (AVX-512BW) ---
    ; bswap reverses byte order of [x3, x2, x1, x0] so that:
    ; bit 0 of bswap(rax) = MSB of x0, ..., bit 63 = LSB of x3!
    bswap      rax
    kmovq      k1, rax            ; load 64 bits directly into AVX-512 opmask k1
    vmovdqa64  zmm0, zmm10        ; start with 64 '0' bytes
    vmovdqu8   zmm0{k1}, zmm11    ; overwrite bytes with '1' where mask bit == 1!
    vmovdqa64  [r13], zmm0        ; single 64-byte aligned store!

    ; --- Step 4: Advance all 4 LCG lanes by 4 steps in parallel (x[i+4]) ---
    ; Lane 0: x[i+4] = (a^4 * x[i]) mod m
    imul eax, r8d, MULTIPLIER_a4
    mov  rcx, rax
    imul rcx, r10
    shr  rcx, MAGIC_SHIFT
    imul ecx, r9d
    sub  eax, ecx
    mov  r8d, eax

    ; Lane 1: x[i+5] = (a^4 * x[i+1]) mod m
    imul eax, r11d, MULTIPLIER_a4
    mov  rcx, rax
    imul rcx, r10
    shr  rcx, MAGIC_SHIFT
    imul ecx, r9d
    sub  eax, ecx
    mov  r11d, eax

    ; Lane 2: x[i+6] = (a^4 * x[i+2]) mod m
    imul eax, r12d, MULTIPLIER_a4
    mov  rcx, rax
    imul rcx, r10
    shr  rcx, MAGIC_SHIFT
    imul ecx, r9d
    sub  eax, ecx
    mov  r12d, eax

    ; Lane 3: x[i+7] = (a^4 * x[i+3]) mod m
    imul eax, r15d, MULTIPLIER_a4
    mov  rcx, rax
    imul rcx, r10
    shr  rcx, MAGIC_SHIFT
    imul ecx, r9d
    sub  eax, ecx
    mov  r15d, eax

    add  r13, 64
    add  r14, 16
    dec  ebx
    jnz  .generate_4x_avx512

    ; Clean up upper ZMM/YMM state before calling glibc functions
    vzeroupper

    ; --- File I/O (Identical to x86-64-v2 version) ---
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

; end of random_streams_for_perf_stats_avx512.asm
