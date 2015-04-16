namespace FsIF.Core

open System

type Memory (size: int) =
    do
        size |> Is.AtLeast 0

    [<Literal>]
    let PageSize = 4096

    let pageCount =
        if size % PageSize > 0 then
            (size / PageSize) + 1
        else
            size / PageSize

    let pages =
        Array.init pageCount (fun _ -> Array.zeroCreate PageSize)

    let mutable currentPage = pages.[0]
    let mutable currentPageStart = 0
    let mutable nextPageStart = PageSize

    let selectPage address =
        let pageIndex = address / PageSize
        currentPage <- pages.[pageIndex]
        currentPageStart <- pageIndex * PageSize
        nextPageStart <- currentPageStart + PageSize

    let readByteCore address : byte=
        if address < currentPageStart || address >= nextPageStart then
            selectPage address

        currentPage.[address - currentPageStart]

    let writeByteCore address value =
        if address < currentPageStart || address >= nextPageStart then
            selectPage address

        currentPage.[address - currentPageStart] <- value

    static let validateAddress address upperBound =
        if address < 0 || address > upperBound then
            raise <| new ArgumentOutOfRangeException("address", address, (sprintf "Expected address to be in the range: 0 to %d" upperBound))

    member x.ReadByte address =
        validateAddress address (size - 1)
        readByteCore address

    member x.ReadWord address =
        validateAddress address (size - 2)

        // We take a faster path if the entire word can be read from the current page
        if address >= currentPageStart && address < (nextPageStart - 2) then
            let page = currentPage
            let pageAddress = address - currentPageStart

            uint16 ((page.[pageAddress] <<< 8) ||| page.[pageAddress + 1])
        else
            let b1 = readByteCore address
            let b2 = readByteCore (address + 1)

            uint16 ((b1 <<< 8) ||| b2)

    member x.ReadDWord address =
        validateAddress address (size - 4)

        // We take a faster path if the entire dword can be read from the current page
        if address >= currentPageStart && address < (nextPageStart - 4) then
            let page = currentPage
            let pageAddress = address - currentPageStart

            uint32 (
                (page.[pageAddress] <<< 24) |||
                (page.[pageAddress + 1] <<< 16) |||
                (page.[pageAddress + 2] <<< 8) |||
                page.[pageAddress + 3])
        else
            let b1 = readByteCore address
            let b2 = readByteCore (address + 1)
            let b3 = readByteCore (address + 2)
            let b4 = readByteCore (address + 3)

            uint32 ((b1 <<< 24) ||| (b2 <<< 16) ||| (b3 <<< 8) ||| b4)

    member x.WriteByte address value =
        validateAddress address (size - 1)
        writeByteCore address value

    member x.WriteWord address value =
        validateAddress address (size - 2)

        let b1 = byte (value >>> 8)
        let b2 = byte (value &&& 0xffus)

        // We take a faster path if the entire word can be written to the current page
        if address >= currentPageStart && address < (nextPageStart - 2) then
            let page = currentPage
            let pageAddress = address - currentPageStart

            page.[pageAddress] <- b1
            page.[pageAddress + 1] <- b2
        else
            writeByteCore address b1
            writeByteCore (address + 1) b2

    member x.WriteDWord address value =
        validateAddress address (size - 4)

        let b1 = byte (value >>> 24)
        let b2 = byte (value >>> 16)
        let b3 = byte (value >>> 8)
        let b4 = byte (value &&& 0xffu)

        // We take a faster path if the entire word can be written to the current page
        if address >= currentPageStart && address < (nextPageStart - 4) then
            let page = currentPage
            let pageAddress = address - currentPageStart

            page.[pageAddress] <- b1
            page.[pageAddress + 1] <- b2
            page.[pageAddress + 2] <- b3
            page.[pageAddress + 3] <- b4
        else
            writeByteCore address b1
            writeByteCore (address + 1) b2
            writeByteCore (address + 2) b3
            writeByteCore (address + 3) b4

    member x.Size = size

    member x.Item
        with get index = x.ReadByte index
        and  set index value = x.WriteByte index value
