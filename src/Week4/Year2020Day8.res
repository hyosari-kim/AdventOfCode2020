let input = Node.Fs.readFileAsUtf8Sync("input/Week4/Year2020Day8.txt")

// parser
// instruction = (key, value, visit)
// string -> array<instruction>

type instruction = Nop | Acc | Jmp

let parser = input' =>
  input'
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(i =>
    switch i->Js.String2.split(" ") {
    | [instruction, value] =>
      switch value->Belt.Int.fromString {
      | Some(value') =>
        switch instruction {
        | "nop" => Some((Nop, value', false))
        | "acc" => Some((Acc, value', false))
        | "jmp" => Some((Jmp, value', false))
        | _ => None
        }
      | _ => None
      }
    | _ => None
    }
  )

type state = {accumulator: int, nextIndex: int, instructions: array<(instruction, int, bool)>}

let update = (instructions, nextIndex) => {
  switch instructions->Belt.Array.get(nextIndex) {
  | Some((instruction, value, _)) =>
    Belt.Array.concatMany([
      instructions->Belt.Array.slice(~offset=0, ~len=nextIndex),
      [(instruction, value, true)],
      instructions->Belt.Array.sliceToEnd(nextIndex + 1),
    ])
  | _ => instructions
  }
}

let next = ({accumulator, nextIndex, instructions}) => {
  switch instructions->Belt.Array.get(nextIndex) {
  | Some((_, _, true)) => {accumulator: accumulator, nextIndex: -1, instructions: instructions}
  | Some((Nop, _, false)) => {
      accumulator: accumulator,
      nextIndex: nextIndex + 1,
      instructions: instructions->update(nextIndex),
    }
  | Some((Acc, value, false)) => {
      accumulator: accumulator + value,
      nextIndex: nextIndex + 1,
      instructions: instructions->update(nextIndex),
    }
  | Some((Jmp, value, false)) => {
      accumulator: accumulator,
      nextIndex: nextIndex + value,
      instructions: instructions->update(nextIndex),
    }
  | _ => {accumulator: accumulator, nextIndex: nextIndex, instructions: instructions}
  }
}

// part 1
let inst = input->parser
let state = {accumulator: 0, nextIndex: 0, instructions: inst}

let rec findInfinLoop = state => {
  let state' = state->next
  switch state'.nextIndex == -1 || state'.nextIndex >= state'.instructions->Belt.Array.length {
  | true => state'
  | false => state'->findInfinLoop
  }
}

let current = state->findInfinLoop
current.accumulator->Js.log

// part2
let instructions = input->parser

let rec change = (nextIndex: int, instructions: array<(instruction, int, bool)>) => {
  switch instructions->Belt.Array.get(nextIndex) {
  | Some((Nop, value, visited)) => (
      nextIndex + 1,
      Belt.Array.concatMany([
        instructions->Belt.Array.slice(~offset=0, ~len=nextIndex),
        [(Jmp, value, visited)],
        instructions->Belt.Array.sliceToEnd(nextIndex + 1),
      ]),
    )
  | Some((Jmp, value, visited)) => (
      nextIndex + 1,
      Belt.Array.concatMany([
        instructions->Belt.Array.slice(~offset=0, ~len=nextIndex),
        [(Nop, value, visited)],
        instructions->Belt.Array.sliceToEnd(nextIndex + 1),
      ]),
    )
  | Some((Acc, _, _)) => change(nextIndex + 1, instructions)
  | _ => (nextIndex + 1, instructions)
  }
}

type loop = Loop | NoLoop(int)

let hasLoop = (instructions: array<(instruction, int, bool)>) => {
  let state = {accumulator: 0, nextIndex: 0, instructions: instructions}

  let finalState = state->findInfinLoop

  switch finalState.nextIndex == -1 {
  | true => Loop
  | false => NoLoop(finalState.accumulator)
  }
}

let rec handleLoop = (nextIndex: int, instructions: array<(instruction, int, bool)>) => {
  let (nextIndex', instructions') = nextIndex->change(instructions)

  switch instructions'->hasLoop {
  | Loop => handleLoop(nextIndex', instructions)
  | NoLoop(acc') => acc'
  }
}

handleLoop(0, instructions)->Js.log
