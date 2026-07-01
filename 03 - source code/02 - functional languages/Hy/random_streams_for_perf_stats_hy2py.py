import hy
import random
END = 62500
m = 65521
a = 17364
c = 0
file_bits_x = 'random_bitstring.bin'
file_bits_hex = 'random_bitstring.byte'
x0 = random.randint(1, m - 1)
print('\ngenerating a random bit stream...')

def masterloop(n, seed):
    """PRNG loop mimicking Clojure's tail-recursive loop/recur structure."""
    acc_nbr_v = []
    current_seed = seed
    bits_x_list = []
    bits_hex_list = []
    for _ in range(n):
        bits_x_str = '{:016b}'.format(current_seed)
        bits_hex_str = '{:04x}'.format(current_seed)
        next_seed = (a * current_seed + c) % m
        acc_nbr_v.append(next_seed)
        current_seed = next_seed
        bits_x_list.append(bits_x_str)
        bits_hex_list.append(bits_hex_str)
    return [acc_nbr_v, ''.join(bits_x_list), ''.join(bits_hex_list)]
results = masterloop(END, x0)
x = results[0]
bits_x = results[1]
bits_hex = results[2]
try:
    _hy_anon_1 = None
    with open(file_bits_x, 'w') as f:
        _hy_anon_1 = f.write(bits_x)
    _hy_anon_2 = print('Bit stream has been written to disk under name: ', file_bits_x)
except Exception as _hy_exc_e_3:
    _hy_anon_2 = print('could not write to file:', file_bits_x, '! --', str(_hy_exc_e_3))
try:
    _hy_anon_4 = None
    with open(file_bits_hex, 'w') as f:
        _hy_anon_4 = f.write(bits_hex)
    _hy_anon_5 = print('Byte stream has been written to disk under name:', file_bits_hex)
except Exception as _hy_exc_e_6:
    _hy_anon_5 = print('could not write to file:', file_bits_hex, '! --', str(_hy_exc_e_6))
