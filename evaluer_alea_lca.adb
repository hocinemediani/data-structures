with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Command_Line;     use Ada.Command_Line;
with Alea;
with LCA;

-- Évaluer la qualité du générateur aléatoire et les LCA.
procedure Evaluer_Alea_LCA is

   package LCA_Alea is
		new LCA (T_Cle => Integer, T_Valeur => Integer);
	use LCA_Alea;


	-- Afficher l'usage.
	procedure Afficher_Usage is
	begin
		New_Line;
		Put_Line ("Usage : " & Command_Name & " Borne Taille");
		New_Line;
		Put_Line ("   Borne  : les nombres sont tirés dans l'intervalle 1..Borne");
		Put_Line ("   Taille : la taille de l'échantillon");
		New_Line;
	end Afficher_Usage;


	-- Afficher le Nom et la Valeur d'une variable.
	-- La Valeur est affichée sur la Largeur_Valeur précisée.
	procedure Afficher_Variable (Nom: String; Valeur: in Integer; Largeur_Valeur: in Integer := 1) is
	begin
		Put (Nom);
		Put (" : ");
		Put (Valeur, Largeur_Valeur);
		New_Line;
	end Afficher_Variable;

	-- Évaluer la qualité du générateur de nombre aléatoire Alea sur un
	-- intervalle donné en calculant les fréquences absolues minimales et
	-- maximales des entiers obtenus lors de plusieurs tirages aléatoires.
	--
	-- Paramètres :
	-- 	  Borne: in Entier	-- le nombre aléatoire est dans 1..Borne
	-- 	  Taille: in Entier -- nombre de tirages (taille de l'échantillon)
	-- 	  Min, Max: out Entier -- fréquence minimale et maximale
	--
	-- Nécessite :
	--    Borne > 1
	--    Taille > 1
	--
	-- Assure : -- poscondition peu intéressante !
	--    0 <= Min Et Min <= Taille
	--    0 <= Max Et Max <= Taille
	--    Min /= Max ==> Min + Max <= Taille
	--
	-- Remarque : On ne peut ni formaliser les 'vraies' postconditions,
	-- ni écrire de programme de test car on ne maîtrise par le générateur
	-- aléatoire.  Pour écrire un programme de test, on pourrait remplacer
	-- le générateur par un générateur qui fournit une séquence connue
	-- d'entiers et pour laquelle on pourrait déterminer les données
	-- statistiques demandées.
	-- Ici, pour tester on peut afficher les nombres aléatoires et refaire
	-- les calculs par ailleurs pour vérifier que le résultat produit est
	-- le bon.
	procedure Calculer_Statistiques (
		Borne    : in Integer;  -- Borne supérieur de l'intervalle de recherche
		Taille   : in Integer;  -- Taille de l'échantillon
		Min, Max : out Integer  -- min et max des fréquences de l'échantillon
	) with
		Pre => Borne > 1 and Taille > 1,
		Post => 0 <= Min and Min <= Taille
			and 0 <= Max and Max <= Taille
			and (if Min /= Max then Min + Max <= Taille)
	is
		package Mon_Alea is
			new Alea (1, Borne);
		use Mon_Alea;

	randInt : Integer;
	LCA : T_LCA;


	function MaxValue(value1, value2 : in Integer) return Integer
		renames Integer'Max;


	function MinValue(value1, value2 : in Integer) return Integer
		renames Integer'Min;


	begin
		if Borne <= 1 or Taille <= 1 then
			Put ("Erreur sur l'appel");
			Min := 0;
			Max := 0;
			return;
		end if;
		Initialiser (LCA);
		for i in 0 .. (Taille - 1) loop
			Get_Random_Number (randInt);
			if Cle_Presente (LCA, randInt) then
				Enregistrer (LCA, randInt, (randInt + 1));
			else
				Enregistrer (LCA, randInt, randInt);
			end if;
		end loop;
		Min := Taille;
		Max := 0;
		for i in 1 .. Borne loop
			if Cle_Presente (LCA, i) then
				Max := MaxValue (Max, La_Valeur (LCA, i));
				Min := MinValue (Min, La_Valeur (LCA, i));
			end if;
		end loop;
		Detruire (LCA);
		end Calculer_Statistiques;



	Min, Max: Integer; -- fréquence minimale et maximale d'un échantillon
	Borne: Integer;    -- les nombres aléatoire sont tirés dans 1..Borne
	Taille: integer;   -- nombre de tirages aléatoires
begin
	if Argument_Count /= 2 then
		Afficher_Usage;
	else
		-- Récupérer les arguments de la ligne de commande
		Borne := Integer'Value (Argument (1));
		Taille := Integer'Value (Argument (2));

		-- Afficher les valeur de Borne et Taille
		Afficher_Variable ("Borne ", Borne);
		Afficher_Variable ("Taille", Taille);

		Calculer_Statistiques (Borne, Taille, Min, Max);

		-- Afficher les fréquence Min et Max
		Afficher_Variable ("Min", Min);
		Afficher_Variable ("Max", Max);
	end if;
end Evaluer_Alea_LCA;
