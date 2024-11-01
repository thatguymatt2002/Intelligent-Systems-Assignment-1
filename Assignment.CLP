(deffunction pause (?delay)		; Function that allows customisable delays within the rest of the program
   (bind ?start (time))
   (while (< (time) (+ ?start ?delay)) do))

(defrule start-invertebrate-length-prompt	; This facilitates the looping of the next rule until a valid input is given
    =>
    (assert (prompt-invertebrate-length)))

(defrule ask-invertebrate-length
    ?prompt <- (prompt-invertebrate-length)
    =>
    (printout t "What is the length of the invertebrate? (in mm)" crlf)
    (bind ?input (read))
    (if (integerp ?input) then ; If the input is an integer
        (retract ?prompt)
        (assert (invertebrate-length ?input)) ; Take in length as input
    else			; If input is not an integer
        (printout t "That is not a valid number. Please try again." crlf)
        (retract ?prompt)
        (assert (length-invalid-input))))	; Fires the next rule

(defrule handle-length-invalid-input	; This rule loops back and refires the first rule
    ?invalid <- (length-invalid-input)
    =>
    (retract ?invalid)
    (assert (prompt-invertebrate-length))
    (pause 3))

(defrule invertebrate-too-short		; If the inputted value is too short to be an invertebrate then this rule is fired
    (invertebrate-length ?length)
    (test (< ?length 2))
    =>
    (printout t "The invertebrate is too short" crlf)
    (pause 5)
    (exit))

(defrule has-shell-or-case
    (invertebrate-length ?length)
    (test (>= ?length 2))
    =>
    (printout t "Does the invertebrate have a shell or case? (yes/no)" crlf)
    (assert (has-shell-or-case (read))))

(defrule what-is-shell-made-of
    (has-shell-or-case yes)
    =>
    (printout t "Is the case made of leaves, twigs, or sand stones? (yes/no)" crlf)
    (assert (case-made-of (read))))

(defrule is-cased-caddis
    (has-shell-or-case yes)	; As this is implementing a decision tree we can use forward chaining as an eventual output will have at least one unique fact
    (case-made-of yes)		; All previous facts that must be certain values are also included to ensure that rules are fired correctly however the program would probably work just fine using just the last fact asserted
    =>
    (printout t "The invertebrate is a cased caddis fly larvae" crlf)
    (pause 5)
    (exit))

(defrule is-two-part-shell
    (has-shell-or-case yes)
    (case-made-of no)
    =>
    (printout t "Does it have a two part shell? (yes/no)" crlf)
    (assert (two-part-shell (read))))

(defrule is-Bi-valve
    (has-shell-or-case yes)
    (two-part-shell yes)
    =>
    (printout t "The invertebrate is a Bi-valve" crlf)
    (pause 5)
    (exit))

(defrule has-coiled-shell
    (has-shell-or-case yes)
    (two-part-shell no)
    =>
    (printout t "Is the shell coiled? (spiral/helical/not coiled)" crlf) ; Could do with some input validation for the rest of the questions to avoid mistypes
    (assert (shell-coiled (read))))

(defrule spiral-shell
    (has-shell-or-case yes)
    (two-part-shell no)
    (shell-coiled spiral)
    =>
    (printout t "The invertebrate is a ramshorn snail" crlf)
    (pause 5)
    (exit))

(defrule helical-shell
    (has-shell-or-case yes)
    (two-part-shell no)
    (shell-coiled helical)
    =>
    (printout t "The invertebrate is a pond snail" crlf)
    (pause 5)
    (exit))

(defrule not-coiled-shell
    (has-shell-or-case yes)
    (two-part-shell no)
    (shell-coiled not)
    =>
    (printout t "The invertebrate is a freshwater limpet" crlf)
    (pause 5)
    (exit))

(defrule has-legs
    (has-shell-or-case no)
    =>
    (printout t "Does the invertebrate have legs? (yes/no)" crlf)
    (assert (legs (read))))

(defrule has-jointed-legs
    (has-shell-or-case no)
    (legs yes)
    =>
    (printout t "Does the invertebrate have jointed legs? (yes/no)" crlf)
    (assert (jointed-legs (read))))

(defrule arthropod-identifier
    (has-shell-or-case no)
    (legs yes)
    (jointed-legs yes)
    =>
    (printout t "You should now consult an Arthropod identifier instead" crlf)
    (pause 5)
    (exit))

(defrule is-legged-fly-larvae
    (has-shell-or-case no)
    (legs yes)
    (jointed-legs no)
    =>
    (printout t "The invertebrate is a legged fly larvae" crlf)
    (pause 5)
    (exit))

(defrule has-looping-movement
    (has-shell-or-case no)
    (legs no)
    =>
    (printout t "Does the invertebrate move in a looping motion? (yes/no)" crlf)
    (assert (looping-movement (read))))

(defrule is-leech
    (has-shell-or-case no)
    (legs no)
    (looping-movement yes)
    =>
    (printout t "The invertebrate is a leech" crlf)
    (pause 5)
    (exit))

(defrule has-flat-body
    (has-shell-or-case no)
    (legs no)
    (looping-movement no)
    =>
    (printout t "Does the invertebrate have a flat body? (yes/no)" crlf)
    (assert (flat-body (read))))

(defrule is-flatworm
    (has-shell-or-case no)
    (legs no)
    (looping-movement no)
    (flat-body yes)
    =>
    (printout t "The invertebrate is a flatworm" crlf)
    (pause 5)
    (exit))

(defrule start-has-segments-prompt	; The next three rules work much the same as the length input validation
    (has-shell-or-case no)
    (legs no)
    (looping-movement no)
    (flat-body no)
    =>
    (assert (prompt-segments)))

(defrule has-segments
    ?prompt <- (prompt-segments)
    =>
    (printout t "How many segments (if any) does the invertebrate have? (0 for no segements)" crlf)
    (bind ?input (read))
    (if (integerp ?input) then
        (retract ?prompt)
        (assert (invertebrate-segments ?input))
    else
        (printout t "That is not a valid number. Please try again." crlf)
        (retract ?prompt)
        (assert (segment-invalid-input))))

(defrule handle-segment-invalid-input
    ?invalid <- (segment-invalid-input)
    =>
    (retract ?invalid)
    (assert (prompt-segments))
    (pause 3))

(defrule is-segmented-fly-larvae
    (has-shell-or-case no)
    (legs no)
    (looping-movement no)
    (flat-body no)
    (invertebrate-segments ?segments)
    (test (<= ?segments 14))
    (test (> ?segments 0))
    =>
    (printout t "The invertebrate is a segmented fly larvae" crlf)
    (pause 5)
    (exit))

(defrule is-worms
    (has-shell-or-case no)
    (legs no)
    (looping-movement no)
    (flat-body no)
    (invertebrate-segments ?segments)
    (test (> ?segments 14))
    =>
    (printout t "The invertebrate is a worm" crlf)
    (pause 5)
    (exit))

(defrule has-tentacles
    (has-shell-or-case no)
    (legs no)
    (looping-movement no)
    (flat-body no)
    (invertebrate-segments 0)
    =>
    (printout t "Does the invertebrate have tentacles on end of body? (yes/no)" crlf)
    (assert (tentacles (read))))

(defrule is-hydra
    (has-shell-or-case no)
    (legs no)
    (looping-movement no)
    (flat-body no)
    (invertebrate-segments 0)
    (tentacles yes)
    =>
    (printout t "The invertebrate is a hydra" crlf)
    (pause 5)
    (exit))

(defrule is-hairworm
    (has-shell-or-case no)
    (legs no)
    (looping-movement no)
    (flat-body no)
    (invertebrate-segments 0)
    (tentacles no)
    =>
    (printout t "The invertebrate is a hairworm" crlf)
    (pause 5)
    (exit))