2026-07-01: work in progress

<br/>  
  
# Factor

https://factorcode.org/

https://github.com/factor/factor/

https://concatenative.org/wiki/view/Factor/Features/The%20language
 
<br/>

## Installation tips

I took latest (as of 2027-07-02) pre-compiled binaries _factor-linux-x86-64-2026-02-11-19-38.tar.gz_ from here: https://builds.factorcode.org/package?os=linux&cpu=x86.64,
unzipped it and added the following line to my _~/.bashrc_ configuration line, which I then activated with usual command: _source ~/.bashrc_

```
export PATH="$HOME/scripts/Factor/factor-linux-x86-64-2026-02-11-19-38/factor:$PATH"
```

This is very important, because there's another Linux application called _factor_ for something very different! Here, we want language interpreter _factor_ to be the first program to be executed under this name.

Then, at least for Ubuntu 24 LTS, a library still has to be installed, something which is also described in _./Factor/factor-linux-x86-64-2026-02-11-19-38/factor/README.md_:

> The development branch of Factor has switched from GTK2 to GTK3 for the GUI backend.

```
$ sudo apt install libgtk-3-dev
...
$
```

When command _$ factor_ is entered, the Factor Listener window should show up like this for example, being ready to receive the first functions that push themselves on the stack:

![plot](./Factor_Listener.png)

..here the ubiquitous _"Hello, world!" print_ function in its .

<br/>

## Tutorial

I highly recommend to first have a look into the official [Guided tour of Factor](https://docs.factorcode.org/content/article-tour.html) before doing anything more meaningful than "Hello, world!" in Factor. You may directly jump into chapter [Playing with the stack](https://docs.factorcode.org/content/article-tour-stack.html).

tbd

<br/>

tbd

<br/>

##_end
