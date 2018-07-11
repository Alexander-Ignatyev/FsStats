namespace FsStats.Tests

open FsUnit.Xunit

module Utils =
    let getTestExpectedResults eps lowerP =
        let cond = should (equalWithin eps)
        let twoTailed = if lowerP < 0.5 then 2.0 * lowerP else 2.0 * (1.0 - lowerP)
        (cond lowerP, cond (1.0 - lowerP), cond twoTailed)
