with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => tNode, Name => tNodePointer);
	

	procedure Initialiser(Sda: out T_LCA) is
	begin
		Sda.size := 0;
		Sda.head := Null;
	end Initialiser;


	procedure Detruire (Sda : in out T_LCA) is

	current : tNodePointer := Sda.head;
	next : tNodePointer;

	begin
		-- Exploring the ADS to look for non null nodes.
		while current /= null loop
			-- Keeping track of the next node BEFORE freeing the current one.
			next := current.next;
			Free(current);
			current := next;
		end loop;
	end Detruire;


	procedure Afficher_Debug (Sda : in T_LCA) is

	current : tNodePointer := Sda.head;

	begin
		while current /= null loop
			Put("-->[");
			Afficher_Cle(current.key);
			Afficher_Donnee(current.value);
			Put("]");
			current := current.next;
		end loop;
		Put_Line("--E");
	end Afficher_Debug;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
		return Sda.head = null;
	end Est_Vide;


	function Taille (Sda : in T_LCA) return Integer is
	begin
		return Sda.size;
	end Taille;


	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is

	current, previous : tNodePointer := Sda.head;

	begin
		while current /= null loop
			if current.key = Cle then
				current.value := Valeur;
				return;
			end if;
			previous := current;
			current := current.next;
		end loop;
		declare
			newNode : CONSTANT tNodePointer := new tNode' (key => Cle, value => Valeur, next => null);
		begin
			if previous = null then
				Sda.head := newNode;
			else
				previous.next := newNode;
			end if;
			Sda.size := Sda.size + 1;
		end;
	end Enregistrer;


	function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
	
	current : tNodePointer := Sda.head;

	begin
		while current /= null loop
			if current.key = Cle then
				return True;
			end if;
			current := current.next;
		end loop;
		return False;
	end Cle_Presente;


	function La_Valeur (Sda : in T_LCA ; Cle : in T_Cle) return T_Valeur is

	current : tNodePointer := Sda.head;

	begin
		while current /= null loop
		    if current.key = Cle then
				return current.value;
			end if;
			current := current.next;
		end loop;
		raise Cle_Absente_Exception;
	end La_Valeur;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
	
	current, previous : tNodePointer := Sda.head;
	
	begin
	    while current /= null loop
	        if current.key = Cle then
	            if Sda.size = 1 then
	                Sda.head := null;
	            elsif current = Sda.head then
	                Sda.head := current.next;
	            else
	                -- We jump a node (the now empty-ed one).
	                previous.next := current.next;
	            end if;
	            Free (current);
	            Sda.size := Sda.size - 1;
	            return;
	        end if;
	        previous := current;
	        current := current.next;
	    end loop;
	    raise Cle_Absente_Exception;
    end Supprimer;

	procedure Pour_Chaque (Sda : in T_LCA) is
	
	current : tNodePointer := Sda.head;
	
	begin
		while current /= null loop
		    begin
		        Traiter(current.key, current.value);
		    exception
				when others => null;
		    end;
		    current := current.next;
		end loop;
	end Pour_Chaque;

end LCA;