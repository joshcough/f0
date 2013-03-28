namespace f0

module Helpers =

    let rec mkString xs sep: string = match xs with
        | [] -> ""
        | x :: [] -> x.ToString()
        | x :: xs -> x.ToString() + sep + (mkString xs sep)

    let fold<'a, 'b, 'x>(fa: 'a -> 'x, fb: 'b -> 'x)(c:Choice<'a, 'b>): 'x = match c with
        | Choice1Of2 a -> fa a
        | Choice2Of2 b -> fb b