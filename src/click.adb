package body Click is

   --  should be [], but not fixed yet in GCC 11.
   Current_Status : KeyMatrix := [others => [others => False]];
   New_Status : KeyMatrix := [others => [others => False]];
   Since : Natural := 0;
   --   Nb_Bounce : Natural := 5;

   function Update (NewS : KeyMatrix) return Boolean is
   begin
      --  The new state is the same as the current stable state => Do nothing.
      if Current_Status = NewS then
         Since := 0;
         return False;
      end if;

      if New_Status /= NewS then
         --  The new state differs from the previous
         --  new state (bouncing) => reset
         New_Status := NewS;
         Since := 1;
      else
         --  The new state hasn't changed since last
         --  update => towards stabilization.
         Since := Since + 1;
      end if;

      if Since > Nb_Bounce then
         declare
            Tmp : constant KeyMatrix := Current_Status;
         begin
            --  New state has been stable enough.
            --  Latch it and notifies caller.
            Current_Status := New_Status;
            New_Status := Tmp;
            Since := 0;
         end;
         return True;
      else
         --  Not there yet
         return False;
      end if;
   end Update;

   function Get_Events (NewS : KeyMatrix) return Events is
      NumEvt : Natural := 0;
   begin
      if Update (NewS) then
         for I in Current_Status'Range (1) loop
            for J in Current_Status'Range (2) loop
               if (not New_Status (I, J) and then Current_Status (I, J))
                 or else (New_Status (I, J) and then not Current_Status (I, J))
               then
                  NumEvt := NumEvt + 1;
               end if;
            end loop;
         end loop;

         declare
            Evts : Events (Natural range 1 .. NumEvt);
            Cursor : Natural range 1 .. NumEvt + 1 := 1;
         begin
            for I in Current_Status'Range (1) loop
               for J in Current_Status'Range (2) loop
                  if not New_Status (I, J)
                    and then Current_Status (I, J)
                  then
                     --  Pressing I, J
                     Evts (Cursor) := [
                                       Evt => Press,
                                       Col => I,
                                       Row => J
                                      ];
                     Cursor := Cursor + 1;
                  elsif New_Status (I, J)
                    and then not Current_Status (I, J)
                  then
                     --  Release I, J
                     Evts (Cursor) := [
                                       Evt => Release,
                                       Col => I,
                                       Row => J
                                      ];
                     Cursor := Cursor + 1;
                  end if;
               end loop;
            end loop;
            return Evts;
         end;
      end if;

      return [];
   end Get_Events;

   function Get_Matrix return KeyMatrix is
      Read_Status : KeyMatrix := [others => [others => False]];
   begin
      --  Ada 2022 allows for := [], but not there yet in GCC11
      --  Read_Status := [others => [others => False]];

      for Row in Keys.Rows'Range loop
         Keys.Rows (Row).Clear;

         for Col in Keys.Cols'Range loop
            if not Keys.Cols (Col).Set then
               Read_Status (Col, Row) := True;
            end if;
         end loop;
         Keys.Rows (Row).Set;
      end loop;

      return Read_Status;
   end Get_Matrix;
end Click;
