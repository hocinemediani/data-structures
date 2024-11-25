with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LCA;

procedure lca_sujet is

   package T_LCA_sujet is
      new LCA(T_Cle => Unbounded_String, T_Valeur => Integer);

   use T_LCA_sujet;

   Sda : T_LCA;

   -- Retourner une chaîne avec des guillemets autour de S
	function Avec_Guillemets (S: Unbounded_String) return String is
	begin
		return '"' & To_String (S) & '"';
	end;


   -- Surcharge l'opérateur unaire "+" pour convertir une String
	-- en Unbounded_String.
	-- Cette astuce permet de simplifier l'initialisation
	-- de cles un peu plus loin.
	function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;


   -- Total size of the ADS.
   Nb_Cles : CONSTANT Integer := 2;
   Cles : CONSTANT array (1..Nb_Cles) of Unbounded_String
         := (+"un", +"deux");
   

   procedure Afficher_Cle (T_Cle : in Unbounded_String) is
   begin
      Put(Avec_Guillemets(T_Cle));
      Put(" : ");
   end Afficher_Cle;


   procedure Afficher_Donnee (T_Valeur : in Integer) is
   begin
      Put(T_Valeur, 1);
   end Afficher_Donnee;


   -- Instanciating Afficher_Debug
   procedure Afficher_Debug_Sda is
      new Afficher_Debug(Afficher_Cle, Afficher_Donnee);


   -- Start of the procedure.
   begin
      Initialiser (Sda);
      Enregistrer (Sda, Cles(1), 1);
      Enregistrer (Sda, Cles(2), 2);
      Afficher_Debug_Sda(Sda);
      Detruire (Sda);

end lca_sujet;
