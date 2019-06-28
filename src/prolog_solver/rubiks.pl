% %
% Solving Core for Rubiks
% %
%   T
% L F R
%   G
%   B
% Cube format is cube(Tx9, Fx9, Lx9, Rx9, Gx9, Bx9)

% FACT 1 - A solved cube has all the faces in the right place
solved(cube(T,T,T,T,T,T,T,T,T,F,F,F,F,F,F,F,F,F,L,L,L,L,L,L,L,L,L,R,R,R,R,R,R,R,R,R,G,G,G,G,G,G,G,G,G,B,B,B,B,B,B,B,B,B)).

% FACT 2 - moves can be made twisting a face of the cube
move(top,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, F1, F2, F3, F4, F5, F6, F7, F8, F9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, G1, G2, G3, G4, G5, G6, G7, G8, G9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(T7, T4, T1, T8, T5, T2, T9, T6, T3, R1, R2, R3, F4, F5, F6, F7, F8, F9, F1, F2, F3, L4, L5, L6, L7, L8, L9, B1, B2, B3, R4, R5, R6, R7, R8, R9, G1, G2, G3, G4, G5, G6, G7, G8, G9, L1, L2, L3,, B4, B5, B6, B7, B8, B9) ).

move(bottom,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, F1, F2, F3, F4, F5, F6, F7, F8, F9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, G1, G2, G3, G4, G5, G6, G7, G8, G9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, F1, F2, F3, F4, F5, F6, L7, L8, L9, L1, L2, L3, L4, L5, L6, B7, B8, B9, R1, R2, R3, R4, R5, R6, F7, F8, F9, G7, G4, G1, G8, G5, G2, G9, G6, G3, B1, B2, B3, B4, B5, B6, R7, R8, R9) ).

move(right,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, F1, F2, F3, F4, F5, F6, F7, F8, F9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, G1, G2, G3, G4, G5, G6, G7, G8, G9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(T1, T2, F3, T4, T5, F6, T7, T8, F9, F1, F2, B3, F4, F5, B6, F7, F8, B9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R7, R4, R1, R8, R5, R2, R9, R6, R3, G1, G2, B3, G4, G5, B6, G7, G8, B9, B1, B2, T3, B4, B5, T6, B7, B8, T9) ).

% Up to here
move(left,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, F1, F2, F3, F4, F5, F6, F7, F8, F9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, G1, G2, G3, G4, G5, G6, G7, G8, G9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(B1, T2, T3, B4, T5, T6, B7, T8, T9, T1, F2, T3, T4, F5, F6, T7, F8, F9, L7, L4, L1, L8, L5, L2, L9, L6, L3, R1, R2, R3, R4, R5, R6, R7, R8, R9, F1, G2, G3, F4, G5, G6, F7, G8, G9, G1, B2, B3, G4, B5, B6, G7, B8, B9) ).

move(front,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, F1, F2, F3, F4, F5, F6, F7, F8, F9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, G1, G2, G3, G4, G5, G6, G7, G8, G9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(T1, T2, T3, T4, T5, T6, L9, L6, L3, F7, F4, F1, F8, F5, F2, F9, F6, F3, L1, L2, G1, L4, L5, G2, L7, L8, G3, T7, R2, R3, T8, R5, R6, T9, R8, R9, R7, R4, R1, G4, G5, G6, G7, G8, G9, B1, B2, B3, B4, B5, B6, B7, B8, B9) ).

move(back,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, F1, F2, F3, F4, F5, F6, F7, F8, F9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, G1, G2, G3, G4, G5, G6, G7, G8, G9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(L7, L4, L1, T4, T5, T6, T7, T8, T9, F1, F2, F3, F4, F5, F6, F7, F8, F9, G3, L2, L1, G6, L5, L2, G9, L8, L3, R1, R2, T1, R4, R5, T2, R7, R8, T3, G1, G2, G3, G4, G5, G6, R9, R6, R3, B7, B4, B1, B8, B5, B2, B9, B6, B3) ).

% FACT 3 - Moving a cube to the same state required no moves