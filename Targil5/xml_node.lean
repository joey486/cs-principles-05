import Std.Data.HashMap
open Std

namespace VMGenerator

inductive XMLNode where
  | elem : String → List XMLNode → XMLNode
  | text : String → XMLNode
  deriving Repr, Nonempty

partial def parseTag (line : String) : Option String :=
  let trimmed := line.trim
  if trimmed.startsWith "<" && !trimmed.startsWith "</" then
    let afterLT := trimmed.drop 1
    let tagName := afterLT.takeWhile (fun c => c != '>' && c != ' ')
    if tagName.length > 0 then some tagName else none
  else none

partial def parseXMLLines (lines : List String) (indent : Nat := 0) : (XMLNode × List String) :=
  match lines with
  | [] => (XMLNode.text "", [])
  | l :: rest =>
    let spaceCount := l.length - l.trimLeft.length
    let lineIndent := spaceCount / 2

    -- If this line has less indentation than expected, we're done with this level
    if lineIndent < indent then
      (XMLNode.text "", lines)
    else
      let trimmed := l.trim
      -- If this is a closing tag, we're done
      if trimmed.startsWith "</" then
        (XMLNode.text "", lines)
      else match parseTag l with
      | some tag =>
        let closingTag := s!"</{tag}>"
        let rec parseChildren (ls : List String) (acc : List XMLNode) :=
          match ls with
          | [] => (acc.reverse, [])
          | x :: xs =>
            let xTrimmed := x.trim
            if xTrimmed == closingTag then
              (acc.reverse, xs)
            else if xTrimmed.startsWith "</" then
              -- If we hit any closing tag, stop parsing children
              (acc.reverse, ls)
            else
              let xSpaceCount := x.length - x.trimLeft.length
              let xLineIndent := xSpaceCount / 2
              -- Only parse as child if indentation is greater than current
              if xLineIndent > indent then
                let (child, rem) := parseXMLLines ls (indent + 1)
                match rem with
                | [] => (acc.reverse, [])
                | _ => parseChildren rem (child :: acc)
              else
                -- If indentation is not greater, we're done with children
                (acc.reverse, ls)
        let (children, remLines) := parseChildren rest []
        (XMLNode.elem tag children, remLines)
      | none =>
        -- Handle text content
        if trimmed.length > 0 then
          (XMLNode.text trimmed, rest)
        else
          -- Skip empty lines
          parseXMLLines rest indent

end VMGenerator
