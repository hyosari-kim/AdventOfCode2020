open Belt
let input = Node.Fs.readFileAsUtf8Sync("input/Week1/input.txt")

// let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.sample.txt")

// input을 개행문자로 나눠 배열을 만들고
// 빈 문자열을 제외한 후
// 배열의 원소인 문자열을 쪼개서 배열로 만든다.

let grid =
  input
  ->Js.String2.split("\n")
  ->Belt.Array.keep(x => x != "")
  ->Belt.Array.map(x => Js.String2.split(x, ""))

// gird 높이와 너비를 구한다
// 한칸씩 내려갈 때마다 오른쪽 좌표는 (현재 내려온 step *3)% width로 이동한다.
// 이동한 위치의 값을 반환한다.
// 배열에서 나무만 keep 한다.
// 최종 배열의 length 를 return 한다.

let height = grid->Belt.Array.length
let width = grid->Belt.Array.getExn(0)->Belt.Array.length

let road =
  Belt.Array.range(1, height - 1)->Belt.Array.map(s =>
    Belt.Array.getExn(grid, s)->Belt.Array.getExn(mod(s * 3, width))
  )
let count = road->Belt.Array.keep(x => x == "#")->Belt.Array.length

road->Js.log
count->Js.log

// Part2
/**
 * Right 1, down 1.
 * Right 3, down 1. (This is the slope you already checked.)
 * Right 5, down 1.
 * Right 7, down 1.
 * Right 1, down 2.

 * multiply of each count
 *
 * 1. slope 를 내려가는 방식을 Tuple로 정리한다
 * 2. 현재 위치 기준 정해진 단위로 내려간뒤에 해당 위치의 grid 값을 return 한다.
 * 3. tree 가 몇개있는지 count 한다
* 4. count 값들을 곱한다.
 */

let step = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
let roads = step->Belt.Array.map(((right, down)) => {
  let rec downSlop = (~r, ~d, ~path) => {
    if d >= height {
      path
    } else {
      let n_path = Belt.Array.concat(
        path,
        [grid->Belt.Array.getExn(d)->Belt.Array.getExn(mod(r, width))],
      )
      downSlop(~r=r + right, ~d=d + down, ~path=n_path)
    }
  }

  downSlop(~r=right, ~d=down, ~path=[])
})

let counts =
  roads->Belt.Array.map(r => r->Belt.Array.reduce(0, (acc, c) => c == "#" ? acc + 1 : acc))

counts->Js.log
let result = counts->Belt.Array.reduce(1.0, (acc, c) => acc *. Js.Int.toFloat(c))->Js.log
