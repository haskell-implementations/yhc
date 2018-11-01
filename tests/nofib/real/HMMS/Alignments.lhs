
        The purpose of executing the Viterbi algorithm is to get an
{\em alignment\/} between the states of the HMM that models an
utterance and the feature vectors generated by the signal processing
module.  The programs that analyze alignment files in order to compute
new HMM network models or compute statistics need to be able to read
the alignment data.
        \begin{haskell}{Alignments}

> module Alignments(
>       FrameData, Alignment, readAlignment,
>       strip_off_frame_number
>       ) where

> import MaybeStateT
> import PlainTextIO

> import Phones
> import HmmDigraphs
> import Char(isSpace)--1.3

\end{haskell}


        Formally, an {\em alignment\/}\index{alignment} is a sequence
of triples.  The first element is the feature vector number, the
second element is the phonetic symbol (i.e., which HMM), and the third
component is the HMM state.
        \begin{haskell}{Alignment}

> type FrameData = (Int, Phone, HmmState)
> type Alignment = [FrameData]

\end{haskell}


        The function \verb~readAlignment~ reads alignment data from a
file.  It is assumed that the file contains nothing but alignment
data.
        \begin{haskell}{readAlignment}

> readAlignment :: [Char] -> Alignment
> readAlignment cs =
>       let
>         cs' = dropWhile isSpace cs
>       in
>         case  readAlignmentFrame cs  of
>         Nothing        -> if null cs'
>                           then []
>                           else error "unparsable chars"
>         Just (f, cs'') -> f : readAlignment cs''             


\end{haskell}


        \begin{haskell}{readAlignmentFrame}

> readAlignmentFrame :: MST [Char] FrameData
> readAlignmentFrame = readsItem        `thenMST` \ i ->
>                      readsItem        `thenMST` \ p ->
>                      readsItem        `thenMST` \ s ->
>                      returnMST (i, p, s)

\end{haskell}


        \begin{haskell}{strip_off_frame_number}

> strip_off_frame_number :: [(a,b,c)] -> [(b,c)]
> strip_off_frame_number = map drop_frame_number

> drop_frame_number :: (a,b,c) -> (b,c)
> drop_frame_number (_,b,c) = (b,c)

\end{haskell}
