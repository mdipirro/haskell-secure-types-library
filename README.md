# Ensuring Information Security by Using Haskell's Advanced Type System
Library presented in **Ensuring Information Security by Using Haskell's Advanced Type System**. 

*Security* contains the actual library implementation and has no dependencies. *Src* contains an application example as described in the paper. It depends on [`aeson`](https://hackage.haskell.org/package/aeson), [`aeson-pretty`](https://hackage.haskell.org/package/aeson-pretty) and [`text`](https://hackage.haskell.org/package/text). For running the application you should start `Main.hs`. 

`Bench.hs` is used as an entrypoint for benchmarking. Benchmarks are made using [`criterion`](https://hackage.haskell.org/package/criterion). You can generate a complete benchmark report as follows:

    ghc -O --make ./Bench
    ./Bench --output ./bench.html

And you can remove files generated during the compilation as follows:

    rm Security/*.hi Security/\*.o
    find . -name '*.o' -delete
    find . -name '*.hi' -delete

Benchmarks are based on the application example, so you will need to install the three packages described above.
