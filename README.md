# kithara
Music library on Haskell.

# Why?

This is my first project on Haskell and also the first one on music.
So the main goal for me here is to learn Haskell and basics of music
(I didn't even know the notes before I started to write `kithara`).

# Resources

Want to write your own synthesizer? (Not necessarily on Haskell.)
Check this out:

- [Code-It-Yourself! Sound Synthesizer](https://www.youtube.com/watch?v=tgamhuQnOkM&list=PLrOv9FMX8xJE8NgepZR1etrsU63fDDGxO&index=6&t=2s&ab_channel=javidx9) series
- [Making Music with Haskell From Scratch](https://youtu.be/FYTZkE5BZ-0)
- [Physics of Music](https://pages.mtu.edu/~suits/Physicsofmusic.html)

# What is done so far and future plans

Right now you already can produce sounds with `kithara`.
But to do so you need to write some Haskell code.
So it would be nice to throw a file on some custom DSL to `kithara`
and then get an audio file. Designing this DSL and implementing it would be the major milestone.

Also it would be nice to add some more sophisticated features like "ADSR" curves of real synthesizers
and other tools. (Right now there is support for a basic ADSR curve with 5 wave shapes: sinusoid, square, triangle, smooth and sharp saws.)
