let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.txt")

// Part 1. 세관신고 그룹 단위 yes 문항 수 세기.
/**
  비행기 착륙전 세관신고서를 작성하려고 한다.
  26개의 문항 (a to z) 는 yes or no 로 체크하게 되어 있다.
  비행기에 딴 사람들로 이루어진 그룹들은 문항을 작성하는데
  그룹 단위 yes 라고 작성한 문항들의 합을 구하라
  중복된 것은 제외한다.
*/

// 1. 그룹단위로 split (blank line)
// 2. 그룹 안에서 문항단위 split (newline 제거)
// 3. 그룹별 Set 을 만들어 yes 문항
// 4. 그룹별 Set length 구하기
// 5. length sum

input
->Js.String2.split("\n\n")
->Js.Vector.toList
->Belt.List.map(g =>
  g
  ->Js.String2.split("")
  ->Belt.Array.keep(s => s != "\n")
  ->Belt.Set.String.fromArray
  ->Belt.Set.String.size
)
->Belt.List.reduce(0, (acc, a) => acc + a)
->Js.log

//Part 2. 그룹내 모든 사람이 yes 한 문항 갯수 구하기
// 1. input 을 그룹으로 split ("/n/n")
// 2. 그룹 안에서 사람단위 나눔
// 3. 사람안에서 문항단위로 split -> Set 으로 만듬
// 4. 사람들이 공통으로 가지고 있는 문항만 남김 : intersect
// 5. 그룹별 문항 length
// 6. sum

input
->Js.String2.split("\n\n")
->Belt.Array.map(group => {
  let people =
    group
    ->Js.String2.split("\n")
    ->Belt.Array.map(str => str->Js.String2.split(""))
    ->Belt.Array.map(p => p->Belt.Set.String.fromArray)

  let initSet = Belt.Set.String.fromArray(["a...z"])

  let intersectSet = people->Belt.Array.reduce(initSet, (acc, p) => {
    acc->Belt.Set.String.intersect(p)
  })

  // list = [1,2,3,4,5,6]
  // list->reduce(0, (acc, p) => acc + p)
  // acc -> 0 + 1 = 1
  // acc -> 1 + 2 = 3
  // acc -> 3 + 3 = 6
  // acc -> (((0 + 1) + 2) + 3) + 4 = 10
  // ....

  intersectSet->Belt.Set.String.size
})
->Belt.Array.reduce(0, (acc, a) => acc + a)
->Js.log
