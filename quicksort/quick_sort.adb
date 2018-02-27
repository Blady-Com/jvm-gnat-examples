package body Quick_Sort is

    procedure Exchange (Left : in out Item; Right : in out Item) is
	Temporary_Item : Item;
    begin
	Temporary_Item := Left;
	Left := Right;
	Right := Temporary_Item;
    end Exchange;

    procedure Sort (The_Items : in out Items) is

	procedure Sort_Recursive (Left_Index : in Index; 
				  Right_Index : in Index) is
	    Pivot_Item : Item;
	    The_Front : Index;
	    The_Back : Index;
	    Middle_Index : Index;
	begin
	    if Left_Index < Right_Index then

		-- Select the Pivot_Item

		Middle_Index := (Left_Index + Right_Index) / 2;
		if The_Items (Middle_Index) < The_Items (Left_Index) then
		    Exchange (The_Items (Middle_Index), The_Items (Left_Index));
		end if;
		if The_Items (Right_Index) < The_Items (Left_Index) then
		    Exchange (The_Items (Right_Index), The_Items (Left_Index));
		end if;
		if The_Items (Right_Index) < The_Items (Middle_Index) then
		    Exchange (The_Items (Right_Index), 
			      The_Items (Middle_Index));
		end if;
		Pivot_Item := The_Items (Middle_Index);
		Exchange (The_Items (Middle_Index), 
			  The_Items (Index'Pred (Right_Index)));
		The_Front := Index'Succ (Left_Index);
		The_Back := Index'Pred (Right_Index);
		if The_Back /= The_Items'First then
		    The_Back := Index'Pred (The_Back);
		end if;

		-- Partition items to the left and right of the Pivot_Item

		loop
		    while The_Items (The_Front) < Pivot_Item loop
			The_Front := Index'Succ (The_Front);
		    end loop;
		    while Pivot_Item < The_Items (The_Back) loop
			The_Back := Index'Pred (The_Back);
		    end loop;
		    if The_Front <= The_Back then
			if (The_Front = The_Items'Last) or else 
			   (The_Back = The_Items'First) then
			    return;
			else
			    Exchange (The_Items (The_Front), 
				      The_Items (The_Back));
			    The_Front := Index'Succ (The_Front);
			    The_Back := Index'Pred (The_Back);
			end if;
		    end if;
		    exit when (The_Front > The_Back);
		end loop;

		Sort_Recursive (Left_Index, The_Back);
		Sort_Recursive (The_Front, Right_Index);
	    end if;
	end Sort_Recursive;

    begin  -- Sort
	Sort_Recursive (The_Items'First, The_Items'Last);
    end Sort;

end Quick_Sort;  
