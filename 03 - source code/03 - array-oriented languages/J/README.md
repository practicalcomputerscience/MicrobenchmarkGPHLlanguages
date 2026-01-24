2026-01-05: work in progress

# J

https://code.jsoftware.com/wiki/Main_Page

https://code.jsoftware.com/wiki/Category:Home

https://www.jsoftware.com/#/README

https://github.com/jsoftware/jsource

<br/>

I have chosen J over APL and other "truly" array-oriented languages because of its usage of ASCII characters, free (personal) availability, being actively maintained, and not because of J's distinctive features (like **tacit** (point-free) programming, where functions are composed without explicitly naming their arguments): [APL versus A versus J versus K?](https://stackoverflow.com/questions/3887521/apl-versus-a-versus-j-versus-k), 2011 - 2020.

Although, using only ASCII characters for the source code is not a free lunch, since APL's unique symbols ("glyphs") are more visually distinct than J's strings with ASCII characters.

While (also) confusing, J seems to have wide and deep documentation, which may be useful, since also J is not the easiest programming language to learn.

All in all, J seems to be a more modern version of the original APL (A Programming Language): [APL Wiki](https://www.aplwiki.com/wiki/Main_Page), both invented by Kennet E. Iverson, plus Roger Hui for J.

<br/>

---

## Installation tips

I got me file _j9.6_linux64.tar.gz_, where everything is already compiled, from here: [System/Installation/J9.6/Zips](https://code.jsoftware.com/wiki/System/Installation/J9.6/Zips) at chapter "Linux / FreeBSD / OpenBSD", extracted it and expanded my _PATH_ environment variable in the _~/.bashrc_ config file with:

```
TBD
```

Then, I started the _$ jconsole_ and there entered these commands ("J sentences") from chapter "Finish Installation":

```
load 'pacman'
'install' jpkg '*'
exit 0
```

This will install 3 icons on the Ubuntu desktop in the lower right-hand corner.

TBD

<br/>

##_end
