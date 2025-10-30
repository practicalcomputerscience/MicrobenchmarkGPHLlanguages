# Wolfram Language

https://www.wolfram.com/language/

<br/>

### Installation tips for the Wolfram Engine

The free for private use and locale Wolfram Engine is for executing **WolframScripts** (_~.wls_): https://www.wolfram.com/wolframscript/

> WARNING!
> The Wolfram Engine, packed into a Bash installation script (_~.sh_), cannot be installed in latest **Ubuntu** version 25.10 as of 2025-10-30!

This is also true for older versions of the Wolfram Engine than latest version 14.3 (as of 2025-10-30): https://www.wolfram.com/engine/

(Ubuntu version 25.10 with its _Rust Coreutils_ has some major problems apparently: https://www.phoronix.com/news/Ubuntu-25.10-Coreutils-Makeself)

<br/>

However, the installation in **Ubuntu 24 LTS**, as used in this benchmarking environment, worked (with me): 

```
chmod 755 ./WolframEngine_14.3.0_LIN.sh
sudo bash ./WolframEngine_14.3.0_LIN.sh
-----------------------------------------------------------------------------------------
                                 Wolfram Engine 14.3 Installer 
-----------------------------------------------------------------------------------------
...
Installation complete.

$ wolframscript
The Wolfram Engine requires one-time activation on this computer.
...
In[1]:= Exit[]
$
```

(TBD)

<br/>

##_end

