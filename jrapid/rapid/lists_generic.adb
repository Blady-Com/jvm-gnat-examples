--README File for Program Distribution Accompanying
--"Software Construction and Data Structures with Ada 95" 
--by Michael B. Feldman 
--copyright 1996, Addison Wesley Publishing Company
--ISBN 0-201-88795-9

--Comments and questions to mfeldman@seas.gwu.edu

--This program distribution is intended mainly for the 
--convenience of users of the above text. You may use the 
--programs as you choose for general educational purposes, 
--provided:

--(1) you must include this README file in any further 
--    distribution;

--(2) if you modify any of the programs and distribute them
--    further, you may add your name and other identifying 
--    info to the comment block at the top of the program, 
--    but must otherwise leave the comment block unchanged.

--Keep in mind that these programs were published in a 
--copyrighted text and should be treated accordingly.
------------------------------------------------------------------------
--| Generic ADT for one-way linked lists
--| Author: Michael B. Feldman, The George Washington University 
--| Last Modified: January 1996                                     
------------------------------------------------------------------------
WITH Unchecked_Deallocation;
PACKAGE BODY Lists_Generic IS

  PROCEDURE Dispose IS
    NEW Unchecked_Deallocation(Object => Node, Name => Position);

  FUNCTION Allocate (X: ElementType; P: Position) RETURN Position IS
    Result: Position;
  BEGIN
    Result := NEW Node'(Info => X, Link => P);
    RETURN Result;
  EXCEPTION
    WHEN Storage_Error =>
      RAISE OutOfSpace;
  END Allocate;

  PROCEDURE Deallocate (P: IN OUT Position) IS
  BEGIN
    Dispose (X => P);
  END Deallocate;

  PROCEDURE Initialize(L: IN OUT List) IS
    Previous: Position;
    Current : Position;
  BEGIN
    IF L.Head /= NULL THEN
      Current := L.Head;
      WHILE Current /= NULL LOOP
    Previous := Current;
    Current := Current.Link;
    Deallocate(Previous);
      END LOOP;
      L := (Head => NULL, Tail => NULL);
    END IF;
  END Initialize;

  PROCEDURE AddToFront(L: IN OUT List; X: ElementType) IS
  BEGIN
    L.Head := Allocate(X, L.Head);
    IF L.Tail = NULL THEN
      L.Tail := L.Head;
    END IF;
  END AddToFront;

  PROCEDURE AddToRear (L: IN OUT List; X: ElementType) IS
    P: Position;
  BEGIN
    P := Allocate(X, NULL);
    IF L.Head = NULL THEN
      L.Head := P;
    ELSE
      L.Tail.Link := P;
    END IF;
    L.Tail := P;
  END AddToRear;

  FUNCTION IsEmpty (L: List) RETURN Boolean IS
  BEGIN
    RETURN L.Head = NULL;
  END IsEmpty;

  FUNCTION IsFirst (L: List; P: Position) RETURN Boolean IS
  BEGIN
    RETURN (L.Head /= NULL) AND (P = L.Head);
  END IsFirst;

  FUNCTION IsLast (L: List; P: Position) RETURN Boolean IS
  BEGIN
    RETURN (L.Tail /= NULL) AND (P = L.Tail);
  END IsLast;

  FUNCTION IsPastEnd (L: List; P: Position) RETURN Boolean IS
  BEGIN
    RETURN P = NULL;
  END IsPastEnd;

  FUNCTION IsPastBegin (L: List; P: Position) RETURN Boolean IS
  BEGIN
    RETURN P = NULL;
  END IsPastBegin;

  FUNCTION First (L: List) RETURN Position IS
  BEGIN
    RETURN L.Head;
  END First;

  FUNCTION  Last (L: List) RETURN Position IS
  BEGIN
    RETURN L.Tail;
  END Last;

  FUNCTION Retrieve (L: IN List; P: IN Position) RETURN ElementType IS
  BEGIN
    IF IsEmpty(L) THEN
      RAISE EmptyList;
    ELSIF IsPastBegin(L, P) THEN
      RAISE PastBegin;
    ELSIF IsPastEnd(L, P) THEN
      RAISE PastEnd;
    ELSE
      RETURN P.Info;
    END IF;
  END Retrieve;

  PROCEDURE GoAhead (L: List; P: IN OUT Position) IS
  BEGIN
    IF IsEmpty(L) THEN
      RAISE EmptyList;
    ELSIF IsPastEnd(L, P) THEN
      RAISE PastEnd;
    ELSE
      P := P.Link;
    END IF;
  END GoAhead;

  PROCEDURE GoBack (L: List; P: IN OUT Position) IS
    Current: Position;
  BEGIN
    IF IsEmpty(L) THEN
      RAISE EmptyList;
    ELSIF IsPastBegin(L, P) THEN
      RAISE PastBegin;
    ELSIF IsFirst(L, P) THEN
      P := NULL;
    ELSE                    -- see whether P is in the list
      Current := L.Head;
      WHILE (Current /= NULL) AND THEN (Current.Link /= P) LOOP
    Current := Current.Link;
      END LOOP;

      IF Current = NULL THEN -- P was not in the list
        RAISE PastEnd;
      ELSE
        P := Current;        -- return predecessor pointer
      END IF;
    END IF;
  END GoBack;

  PROCEDURE Delete    (L: IN OUT List; P: Position) IS
    Previous: Position;
    Current : Position;
  BEGIN
    Current := P;
    IF IsEmpty(L) THEN
      RAISE EmptyList;
    ELSIF IsPastBegin(L, Current) THEN
      RAISE PastBegin;
    ELSIF IsFirst(L, Current) THEN  -- must adjust list header
      L.Head := Current.Link;
      IF L.Head = NULL THEN         -- deleted the only node
        L.Tail := NULL;
      END IF;
    ELSE                            -- "normal" situation
      Previous := Current;
      GoBack(L, Previous);
      Previous.Link := Current.Link;
      IF IsLast(L, Current) THEN     -- deleted the last node
        L.Tail := Previous;
      END IF;
    END IF;
    Deallocate(Current);
  END Delete;

  PROCEDURE Insert    (L: IN OUT List; X: ElementType; P: Position) IS
  BEGIN
    IF P = NULL THEN
      AddToRear(L, X);
    ELSE
      P.Link := Allocate(X, P.Link);
    END IF;
  END Insert;

  PROCEDURE Replace   (L: IN OUT List; X: ElementType; P: Position) IS
  BEGIN
    IF P = NULL THEN
      RAISE PastEnd;
    ELSE
      P.Info := X;
    END IF;
  END Replace;

  PROCEDURE Copy (To: IN OUT List; From: IN List) IS
    Current: Position;
  BEGIN
    Initialize(To);
    Current := First(From);
    WHILE NOT IsPastEnd(From, Current) LOOP
      AddToRear(To, Retrieve(From, Current));
      GoAhead(From, Current);
    END LOOP;
  END Copy;

END Lists_Generic;