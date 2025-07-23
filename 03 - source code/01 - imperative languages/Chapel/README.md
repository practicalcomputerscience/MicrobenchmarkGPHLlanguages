https://chapel-lang.org/

---

#### Installation tips

Chapel version 2.4.0 is built on LLVM version 18.1.8 (at least in my case).

In case that a LLVM installation with version 18 has become missing, it can be (re-)installed like this: https://ubuntuhandbook.org/index.php/2023/09/how-to-install-clang-17-or-16-in-ubuntu-22-04-20-04/

Apparently, this procedure also applies to other LLVM versions, like 17, 19, 20 and so on.

```
$ wget https://apt.llvm.org/llvm.sh
$ chmod u+x llvm.sh
$ sudo ./llvm.sh 18  # 18 for LLVM version 18
$ sudo mkdir -p /etc/apt/keyrings  # dir /etc/apt/keyrings doesn't exist on my system
$ sudo mv /etc/apt/trusted.gpg.d/apt.llvm.org.asc /etc/apt/keyrings/
$ sudo nano /etc/apt/sources.list.d/archive_uri-http_apt_llvm_org_noble_*.list
```

with the last command I edited this line:

_deb http://apt.llvm.org/noble/ llvm-toolchain-noble-18 main_

into:

_deb [arch=amd64 signed-by=/etc/apt/keyrings/apt.llvm.org.asc] http://apt.llvm.org/noble/ llvm-toolchain-noble-18 main_

Press Ctrl+S to save the changed file, and Ctrl+X to exit the nano editor. At last do:

```
$ sudo apt update
$ clang-18 --version  # check installation
```

##_end
