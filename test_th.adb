with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with SDA_Exceptions; 		use SDA_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TH;

procedure Test_TH is

   arrayLength : CONSTANT Integer := 11;

	package TH_Test is
		new TH (
         nodeKey => Unbounded_String,
         nodeValue => Integer,
         lengthArray => arrayLength
         );
	use TH_Test;


	-- Retourner une chaîne avec des guillemets autour de S
	function Avec_Guillemets (S: Unbounded_String) return String is
	begin
		return '"' & To_String (S) & '"';
	end;


	-- Utiliser & entre String à gauche et Unbounded_String à droite.  Des
	-- guillemets sont ajoutées autour de la Unbounded_String 
	-- Il s'agit d'un masquage de l'opérateur `&` défini dans Strings.Unbounded
	function "&" (Left: String; Right: Unbounded_String) return String is
	begin
		return Left & Avec_Guillemets (Right);
	end;


	-- Surcharge l'opérateur unaire "+" pour convertir une String
	-- en Unbounded_String.
	-- Cette astuce permet de simplifier l'initialisation
	-- de cles un peu plus loin.
	function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;


   Keys : CONSTANT array (0 .. 6) of Unbounded_String
      := (+"un", +"deux", +"trois", +"quatre", +"cinq", +"vingt-et-un", +"quatre-vingt-dix-neuf");
	Inconnu : constant  Unbounded_String := To_Unbounded_String ("Inconnu");

	Donnees : constant array (0..6) of Integer
			:= (1, 2, 3, 4, 5, 21, 99);
	Somme_Donnees : constant Integer := 135;
	Somme_Donnees_Len4 : constant Integer := 7;
	Somme_Donnees_Q: constant Integer := 103;


	-- Initialiser l'annuaire avec les Donnees et Keys ci-dessus.
	-- Attention, c'est à l'appelant de libérer la mémoire associée en
	-- utilisant DestroyHashTable.
	-- Si Bavard est vrai, les insertions sont tracées (affichées).
	procedure Construire_Exemple_Sujet (HashTable : out hashMap; Bavard: Boolean := False) is
	begin
		InitialiseHashTable (HashTable, arrayLength);
		pragma Assert (IsEmpty (HashTable));
		pragma Assert (GetSize (HashTable) = 0);

		for I in 1 .. 6 loop
			Register (HashTable, Keys (I), Donnees (I));

			if Bavard then
				Put_Line ("Après insertion de la clé " & Keys (I));
				DisplayHashTable (HashTable);
				New_Line;
			else
				null;
			end if;

			pragma Assert (not IsEmpty (HashTable));
			pragma Assert (GetSize (HashTable) = I);

			for J in 1 .. I loop
				pragma Assert (ValueOf (HashTable, Keys (J)) = Donnees (J));
			end loop;

			for J in (I + 1) .. 6 loop
				pragma Assert (not IsIn (HashTable, Keys (J)));
			end loop;

		end loop;
	end Construire_Exemple_Sujet;


	procedure Tester_Exemple_Sujet is
		HashTable : hashMap;
	begin
		Construire_Exemple_Sujet (HashTable, True);
		DestroyHashTable (HashTable);
	end Tester_Exemple_Sujet;


	-- Tester suppression en commençant par les derniers éléments ajoutés
	procedure Tester_Delete_Inverse is
		HashTable : hashMap;
	begin
		Put_Line ("=== Tester_Delete_Inverse..."); New_Line;

		Construire_Exemple_Sujet (HashTable);

		for I in reverse 1..6 loop

			Delete (HashTable, Keys (I));

			Put_Line ("Après suppression de " & Keys (I) & " :");
			DisplayHashTable (HashTable); New_Line;

			for J in 1..I-1 loop
				pragma Assert (IsIn (HashTable, Keys (J)));
				pragma Assert (ValueOf (HashTable, Keys (J)) = Donnees (J));
			end loop;

			for J in I..6 loop
				pragma Assert (not IsIn (HashTable, Keys (J)));
			end loop;
		end loop;

		DestroyHashTable (HashTable);
	end Tester_Delete_Inverse;


	-- Tester suppression en commençant les les premiers éléments ajoutés
	procedure Tester_Delete is
		HashTable : hashMap;
	begin
		Put_Line ("=== Tester_Delete..."); New_Line;

		Construire_Exemple_Sujet (HashTable);

		for I in 1 .. 6 loop
			Put_Line ("Suppression de " & Keys (I) & " :");

			Delete (HashTable, Keys (I));

			DisplayHashTable (HashTable); New_Line;

			for J in 1 .. I loop
				pragma Assert (not IsIn (HashTable, Keys (J)));
			end loop;

			for J in (I + 1) ..6 loop
				pragma Assert (IsIn (HashTable, Keys (J)));
				pragma Assert (ValueOf (HashTable, Keys (J)) = Donnees (J));
			end loop;
		end loop;

		DestroyHashTable (HashTable);
	end Tester_Delete;


	procedure Tester_Delete_Un_Element is

		-- Tester supprimer sur un élément, celui à Indice dans Keys.
		procedure Tester_Delete_Un_Element (Indice: in Integer) is
			HashTable : hashMap;
		begin
			Construire_Exemple_Sujet (HashTable);

			Put_Line ("Suppression de " & Keys (Indice) & " :");
			Delete (HashTable, Keys (Indice));

			DisplayHashTable (HashTable); New_Line;

			for J in 1 .. 6 loop
				if J = Indice then
					pragma Assert (not IsIn (HashTable, Keys (J)));
				else
					pragma Assert (IsIn (HashTable, Keys (J)));
				end if;
			end loop;

			DestroyHashTable (HashTable);
		end Tester_Delete_Un_Element;

	begin
		Put_Line ("=== Tester_Delete_Un_Element..."); New_Line;

		for I in 1..6 loop
			Tester_Delete_Un_Element (I);
		end loop;
	end Tester_Delete_Un_Element;


	procedure Tester_Remplacer_Un_Element is

		-- Tester Register sur un élément présent, celui à Indice dans Keys.
		procedure Tester_Remplacer_Un_Element (Indice: in Integer; Nouveau: in Integer) is
			HashTable : hashMap;
		begin
			Construire_Exemple_Sujet (HashTable);

			Put_Line ("Mise à jour de " & Keys (Indice)
					& " avec " & Integer'Image(Nouveau) & " :");

			pragma Assert(IsIn (HashTable, Keys (Indice))); -- bien présent

			Register (HashTable, Keys (Indice), Nouveau); -- valeur remplacée

			DisplayHashTable (HashTable); New_Line;

			for J in 1..6 loop
				pragma Assert (IsIn (HashTable, Keys (J)));
				if J = Indice then
					pragma Assert (ValueOf (HashTable, Keys (J)) = Nouveau);
				else
					pragma Assert (ValueOf (HashTable, Keys (J)) = Donnees (J));
				end if;
			end loop;

			DestroyHashTable (HashTable);
		end Tester_Remplacer_Un_Element;

	begin
		Put_Line ("=== Tester_Remplacer_Un_Element..."); New_Line;

		for I in 1..6 loop
			Tester_Remplacer_Un_Element (I, 0);
			null;
		end loop;
	end Tester_Remplacer_Un_Element;


	procedure Tester_Delete_Erreur is
		HashTable : hashMap;
	begin
		begin
			Put_Line ("=== Tester_Delete_Erreur..."); New_Line;

			Construire_Exemple_Sujet (HashTable);
			Delete (HashTable, Inconnu);

		exception
			when Cle_Absente_Exception =>
				null;
			when others =>
				pragma Assert (False);
		end;
		DestroyHashTable (HashTable);
	end Tester_Delete_Erreur;


	procedure Tester_ValueOf_Erreur is
		HashTable : hashMap;
		Inutile: Integer;
		pragma Unreferenced (Inutile);
	begin
		begin
			Put_Line ("=== Tester_ValueOf_Erreur..."); New_Line;

			Construire_Exemple_Sujet (HashTable);
			Inutile := ValueOf (HashTable, Inconnu);

		exception
			when Cle_Absente_Exception =>
				null;
			when others =>
				pragma Assert (False);
		end;
		DestroyHashTable (HashTable);
	end Tester_ValueOf_Erreur;


	procedure Tester_Pour_chaque is
		HashTable : hashMap;

		Somme: Integer;

		procedure Sommer (Cle: Unbounded_String; Valeur: Integer) is
			pragma Unreferenced (Cle);
		begin
			Put (" + ");
			Put (Valeur, 2);
			New_Line;

			Somme := Somme + Valeur;
		end;

		procedure Sommer is
			new ForAll (Sommer);

	begin
		Put_Line ("=== Tester_ForAll..."); New_Line;
		Construire_Exemple_Sujet(HashTable);
		Somme := 0;
		Sommer (HashTable);
		pragma Assert (Somme = Somme_Donnees);
		DestroyHashTable(HashTable);
		New_Line;
	end Tester_Pour_chaque;


	procedure Tester_Pour_chaque_Somme_Si_Cle_Commence_Par_Q is
		HashTable : hashMap;

		Somme: Integer;

		procedure Sommer_Cle_Commence_Par_Q (Cle: Unbounded_String; Valeur: Integer) is
		begin
			if To_String (Cle) (1) = 'q' then
				Put (" + ");
				Put (Valeur, 2);
				New_Line;

				Somme := Somme + Valeur;
			else
				null;
			end if;
		end;

		procedure Sommer is
			new ForAll (Sommer_Cle_Commence_Par_Q);

	begin
		Put_Line ("=== Tester_ForAll_Somme_Si_Cle_Commence_Par_Q..."); New_Line;
		Construire_Exemple_Sujet(HashTable);
		Somme := 0;
		Sommer (HashTable);
		pragma Assert (Somme = Somme_Donnees_Q);
		DestroyHashTable(HashTable);
		New_Line;
	end Tester_Pour_chaque_Somme_Si_Cle_Commence_Par_Q;



	procedure Tester_Pour_chaque_Somme_Len4_Avec_Exception is
		HashTable : hashMap;

		Somme: Integer;

		procedure Sommer_Len4_Avec_Exception (Cle: Unbounded_String; Valeur: Integer) is
			Nouvelle_Exception: Exception;
		begin
			if Length (Cle) = 4 then
				Put (" + ");
				Put (Valeur, 2);
				New_Line;

				Somme := Somme + Valeur;
			else
				raise Nouvelle_Exception;
			end if;
		end;

		procedure Sommer is
			new ForAll (Sommer_Len4_Avec_Exception);

	begin
		Put_Line ("=== Tester_ForAll_Somme_Len4_Avec_Exception..."); New_Line;
		Construire_Exemple_Sujet(HashTable);
		Somme := 0;
		Sommer (HashTable);
		pragma Assert (Somme = Somme_Donnees_Len4);
		DestroyHashTable(HashTable);
		New_Line;
	end Tester_Pour_chaque_Somme_Len4_Avec_Exception;



begin
	Tester_Exemple_Sujet;
	Tester_Delete_Inverse;
	Tester_Delete;
	Tester_Delete_Un_Element;
	Tester_Remplacer_Un_Element;
	Tester_Delete_Erreur;
	Tester_ValueOf_Erreur;
	Tester_Pour_chaque;
	Tester_Pour_chaque_Somme_Si_Cle_Commence_Par_Q;
	Tester_Pour_chaque_Somme_Len4_Avec_Exception;
	Put_Line ("Fin des tests : OK.");
end Test_TH;