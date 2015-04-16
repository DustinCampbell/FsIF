module MemoryTests

    open FsIF.Core
    open Xunit

    [<Fact>]
    let SizeTest () =
        let m = new Memory(4096)
        Assert.Equal(4096, m.Size)