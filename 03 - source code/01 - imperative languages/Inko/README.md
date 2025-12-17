# Inko

https://inko-lang.org/

---

## Installation tips

Things got much easier with Inko version 0.19.1 compared to the version that I used before, that was version 0.18.1.

I installed Inko version 0.19.1 like this, leaning to chapter **From source** and starting with the latest release tarbal: https://docs.inko-lang.org/manual/latest/setup/installation/

```
$ VER='0.19.1'
$ mkdir $VER
$ curl https://releases.inko-lang.org/$VER.tar.gz -o $VER.tar.gz
$ tar -C $VER -xf $VER.tar.gz
$ cd $VER
$ cargo build --release
$ make
$ sudo make install
$ inko --version
inko 0.19.1
$
```

<br/>

##_end
