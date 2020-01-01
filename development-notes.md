# 12/30/2019

- The project doesn't compile and has uncommitted changes I don't understand. Going to commit them
  and add a travis config.
- Some packages have package.yaml's that are not used.
- I'm on some wierd branch ... that is going to be become master ... at least for now.
- Had to make repo public to get CI ... oh well.
- CI is failing on hspec-discover not being on the path ... twitter probably knows how to fix that.
- Someone on twitter had an answer: https://twitter.com/GeorgeTalksCode/status/1211915225999364096?s=20

# 12/31/2019

- CI is now compiling as much as I am locally. So time to get local compiling. I think I might have quit working on this when I hit a point where `with` would be useful in `tmp-postgres`.
- Scratch that getting an error from `PartialOptions` locally that do not get in CI.
- `postgresql-simple-opts` is busted. I was kinda of hoping I would not have to fix that right away. This is the problem and value of dog fooding I guess.
- Hopingfully I can modify `trek` to not depend on `postgresql-simple-opts`.
- I still can't believe I named the package "opts" instead of "options". I spelled out "postgresql-simple" but "options" was too much ... smh.
- I might be able to fix it by adjusting the dependencies.
