type teaching_unit =
  { code : string
  ; title : string
  ; ects : int
  }

type unit_block =
  { ects : int
  ; units : teaching_unit list
  ; required_children : int
  ; children : unit_block list
  }

type training =
  { code : string
  ; name : string
  ; curriculum : unit_block list
  }

let _ANG100 = { code = "ANG100"; title = "Anglais général pour débutants"; ects = 6 }
let _ANG320 = { code = "ANG320"; title = "Anglais professionnel"; ects = 6 }
let _ANG330 = { code = "ANG330"; title = "Anglais professionnel"; ects = 6 }
let _BCA001 = { code = "BCA001"; title = "Initiation biologie-biochimie structurale"; ects = 6 }
let _BNF104 = { code = "BNF104"; title = "Utilisation et applications de la bio-informatique"; ects = 6 }
let _CFA109 = { code = "CFA109"; title = "Information comptable et management"; ects = 6 }
let _DNT104 = { code = "DNT104"; title = "Droit du numérique"; ects = 4 }
let _DRS101 = { code = "DRS101"; title = "Droit du travail = relations individuelles"; ects = 6 }
let _DRS102 = { code = "DRS102"; title = "Droit du travail = relations collectives"; ects = 6 }
let _DRS106 = { code = "DRS106"; title = "Droit social européen et international"; ects = 6 }
let _DSY101 = { code = "DSY101"; title = "L'organisation & ses modèles = Panorama (1)"; ects = 6 }
let _DVE207 = { code = "DVE207"; title = "Droit et pratique des contrats internationaux"; ects = 6 }
let _ELE015 = { code = "ELE015"; title = "Introduction a l'électronique numérique"; ects = 6 }
let _ENG210 = { code = "ENG210"; title = "Ingénieur de demain"; ects = 6 }
let _ENG221 = { code = "ENG221"; title = "Information et communication pour l'ingénieur - Oral probatoire"; ects = 6 }
let _ENG251 = { code = "ENG251"; title = "Information et communication pour l'ingénieur - Oral probatoire"; ects = 3 }
let _ENG261 = { code = "ENG261"; title = "Information et communication scientifique"; ects = 3 }
let _ERG105 = { code = "ERG105"; title = "Santé; performance et développement au travail"; ects = 6 }
let _ESC101 = { code = "ESC101"; title = "Mercatique I = Les Etudes de marché et les nouveaux enjeux de la Data"; ects = 6 }
let _ESD104 = { code = "ESD104"; title = "Politiques et stratégies économiques dans la mondialisation"; ects = 6 }
let _FAB121 = { code = "FAB121"; title = "Outils et méthodes du Lean"; ects = 6 }
let _FAD111 = { code = "FAD111"; title = "Analyse du travail et ingénierie de la formation professionnelle"; ects = 8 }
let _FPG114 = { code = "FPG114"; title = "Outils RH"; ects = 6 }
let _GDN100 = { code = "GDN100"; title = "Management de projet"; ects = 4 }
let _GFN106 = { code = "GFN106"; title = "Pilotage financier de l'entreprise"; ects = 6 }
let _GLG101 = { code = "GLG101"; title = "Test et Validation du Logiciel"; ects = 6 }
let _GLG105 = { code = "GLG105"; title = "Génie logiciel"; ects = 6 }
let _GLG203 = { code = "GLG203"; title = "Architectures Logicielles Java(1)"; ects = 6 }
let _GLG204 = { code = "GLG204"; title = "Architectures Logicielles Java(2)"; ects = 6 }
let _GLG206 = { code = "GLG206"; title = "Projets informatiques = méthodes et outils (1)"; ects = 6 }
let _GLG207 = { code = "GLG207"; title = "Projets informatiques = méthodes et outils (2)"; ects = 6 }
let _GME101 = { code = "GME101"; title = "Genre et travail"; ects = 6 }
let _HSE133 = { code = "HSE133"; title = "Enjeux des transitions écologiques= comprendre et agir"; ects = 3 }
let _HSE134 = { code = "HSE134"; title = "Intégrer les enjeux de transitions écologiques dans les pratiques professionnelles"; ects = 3 }
let _HSE225 = { code = "HSE225"; title = "Eléments de santé au travail pour les ingénieurs et les managers (ESTIM)"; ects = 3 }
let _MSE102 = { code = "MSE102"; title = "Management et organisation des entreprises"; ects = 6 }
let _MSE103 = { code = "MSE103"; title = "Management et organisation des entreprises - Compléments"; ects = 3 }
let _MSE147 = { code = "MSE147"; title = "Principes généraux et outils du management d'entreprise"; ects = 9 }
let _MTR107 = { code = "MTR107"; title = "Introduction au management qualité"; ects = 3 }
let _MUX101 = { code = "MUX101"; title = "Multimédia et interaction humain-machine"; ects = 6 }
let _MUX102 = { code = "MUX102"; title = "Interaction humain-machine = conception d'interfaces et expérience utilisateur"; ects = 6 }
let _MUX103 = { code = "MUX103"; title = "Design d'interaction pour mobiles"; ects = 6 }
let _MUX104 = { code = "MUX104"; title = "Synthèse d'image et réalité virtuelle"; ects = 6 }
let _MUX205 = { code = "MUX205"; title = "Média numériques avancés = programmation des jeux vidéo"; ects = 6 }
let _MUX206 = { code = "MUX206"; title = "Médias interactifs avancés = game design des jeux vidéo"; ects = 6 }
let _MVA003 = { code = "MVA003"; title = "Outils mathématiques pour l'informatique (Combinatoire; probabilités; ordre; calcul booléen)"; ects = 6 }
let _MVA004 = { code = "MVA004"; title = "Mathématiques pour l'informatique approfondissement (Automates; codes; graphes et matrices)"; ects = 6 }
let _MVA010 = { code = "MVA010"; title = "Bases de l'analyse mathématique"; ects = 4 }
let _NFA003 = { code = "NFA003"; title = "Principes et fonctionnement des systèmes d'exploitation"; ects = 4 }
let _NFA004 = { code = "NFA004"; title = "Architecture des machines"; ects = 4 }
let _NFA006 = { code = "NFA006"; title = "Structures de données"; ects = 4 }
let _NFA007 = { code = "NFA007"; title = "Méthodes pour l'informatisation"; ects = 4 }
let _NFA008 = { code = "NFA008"; title = "Bases de données"; ects = 6 }
let _NFA009 = { code = "NFA009"; title = "Principes des réseaux informatiques"; ects = 6 }
let _NFA010 = { code = "NFA010"; title = "Graphes et optimisation"; ects = 6 }
let _NFA011 = { code = "NFA011"; title = "Développement d'applications avec les bases de données"; ects = 4 }
let _NFA013 = { code = "NFA013"; title = "Méthodes pour l'informatisation - compléments"; ects = 4 }
let _NFA018 = { code = "NFA018"; title = "Gestion de projet informatique"; ects = 4 }
let _NFA019 = { code = "NFA019"; title = "Projet systèmes d'information = mise en pratique avec Java"; ects = 6 }
let _NFA021 = { code = "NFA021"; title = "Développement web (3) = mise en pratique"; ects = 6 }
let _NFA022 = { code = "NFA022"; title = "Principes et programmation système et réseau pour  smart-phones et tablettes tactiles"; ects = 4 }
let _NFA024 = { code = "NFA024"; title = "Projet application mobile = mise en pratique"; ects = 6 }
let _NFA025 = { code = "NFA025"; title = "Mise en oeuvre de la programmation de smart-phones et tablettes tactiles"; ects = 6 }
let _NFA031 = { code = "NFA031"; title = "Programmation avec Java = notions de base"; ects = 6 }
let _NFA032 = { code = "NFA032"; title = "Programmation  Java = programmation objet"; ects = 6 }
let _NFA035 = { code = "NFA035"; title = "Programmation Java = bibliothèques et patterns"; ects = 4 }
let _NFA040 = { code = "NFA040"; title = "Architecture et langages Web"; ects = 4 }
let _NFA041 = { code = "NFA041"; title = "Programmation Javascript"; ects = 6 }
let _NFA042 = { code = "NFA042"; title = "Développement web côté serveur"; ects = 6 }
let _NFE101 = { code = "NFE101"; title = "Ingénierie d'intégration et d'évolution des systèmes d'information"; ects = 6 }
let _NFE103 = { code = "NFE103"; title = "Méthodologies avancées d'informatisation"; ects = 6 }
let _NFE106 = { code = "NFE106"; title = "Ingénierie et optimisation des bases de données"; ects = 6 }
let _NFE107 = { code = "NFE107"; title = "Architecture d'Entreprise et Urbanisation des Systèmes d'Information"; ects = 6 }
let _NFE108 = { code = "NFE108"; title = "Méthodologies des systèmes d'information"; ects = 6 }
let _NFE109 = { code = "NFE109"; title = "Ingénierie des processus et systèmes d'information"; ects = 6 }
let _NFE113 = { code = "NFE113"; title = "Conception et administration de bases de données"; ects = 6 }
let _NFE114 = { code = "NFE114"; title = "Systèmes d'information web"; ects = 6 }
let _NFE115 = { code = "NFE115"; title = "Introduction à la gestion de données à large échelle"; ects = 6 }
let _NFE130 = { code = "NFE130"; title = "Audit des systèmes d'information"; ects = 6 }
let _NFE155 = { code = "NFE155"; title = "ITIL et la gestion des services des systèmes d'information"; ects = 6 }
let _NFE204 = { code = "NFE204"; title = "Bases de données documentaires et distribuées"; ects = 6 }
let _NFE205 = { code = "NFE205"; title = "Données multimédia et spatio-temporelles"; ects = 6 }
let _NFE209 = { code = "NFE209"; title = "Ingénierie des systèmes d'information - Stratégie et gouvernance du SI et des données; audit informatique"; ects = 6 }
let _NFE210 = { code = "NFE210"; title = "Ingénierie des systèmes d'information - Méthodes avancées de pilotage du SI"; ects = 6 }
let _NFE211 = { code = "NFE211"; title = "Business Intelligence (1)  - Data Warehouses"; ects = 6 }
let _NFE212 = { code = "NFE212"; title = "Business Intelligence (2) - Visualisation et Valorisation"; ects = 6 }
let _NFP101 = { code = "NFP101"; title = "Programmation orientée objet en Python; Java et autres"; ects = 6 }
let _NFP103 = { code = "NFP103"; title = "Spécification et vérification des systèmes distribués"; ects = 6 }
let _NFP106 = { code = "NFP106"; title = "Intelligence artificielle"; ects = 6 }
let _NFP107 = { code = "NFP107"; title = "Systèmes de gestion de bases de données"; ects = 6 }
let _NFP108 = { code = "NFP108"; title = "Spécification et Modélisation Informatiques"; ects = 6 }
let _NFP119 = { code = "NFP119"; title = "Programmation Fonctionnelle = des concepts aux applications web"; ects = 6 }
let _NFP121 = { code = "NFP121"; title = "Programmation avancée"; ects = 6 }
let _NSY014 = { code = "NSY014"; title = "Applications réparties"; ects = 6 }
let _NSY102 = { code = "NSY102"; title = "Intergiciels à objets répartis = conception et implantation"; ects = 6 }
let _NSY103 = { code = "NSY103"; title = "Linux = principes et programmation"; ects = 6 }
let _NSY104 = { code = "NSY104"; title = "Architectures des systèmes informatiques"; ects = 6 }
let _NSY107 = { code = "NSY107"; title = "Architectures Cloud; intégration des applications et sécurité."; ects = 6 }
let _NSY115 = { code = "NSY115"; title = "Conduite d'un projet informatique"; ects = 6 }
let _NSY135 = { code = "NSY135"; title = "Applications orientées données - patrons; frameworks; ORM"; ects = 6 }
let _NSY205 = { code = "NSY205"; title = "Architectures et technologies pour l'intégration des systèmes"; ects = 6 }
let _NSY206 = { code = "NSY206"; title = "Méthodologie d'ingénierie et d'intégration des systèmes"; ects = 6 }
let _NSY208 = { code = "NSY208"; title = "Architecture; Patterns; et Intégration = systèmes embarqués et mobiles en Java et Android (1)"; ects = 6 }
let _NSY209 = { code = "NSY209"; title = "Architecture; Patterns; et Intégration = systèmes embarqués et mobiles en Java et Android (2)"; ects = 6 }
let _NTD217 = { code = "NTD217"; title = "Principes et fondamentaux de la gouvernance des connaissances"; ects = 3 }
let _PRS201 = { code = "PRS201"; title = "Prospective; décision; transformation"; ects = 6 }
let _RCP101 = { code = "RCP101"; title = "Recherche opérationnelle et aide à la décision"; ects = 6 }
let _RCP103 = { code = "RCP103"; title = "Evaluation de performances et sûreté de fonctionnement"; ects = 6 }
let _RCP104 = { code = "RCP104"; title = "Optimisation en informatique"; ects = 6 }
let _RCP105 = { code = "RCP105"; title = "Modélisation; optimisation; complexité et algorithmes"; ects = 6 }
let _RCP106 = { code = "RCP106"; title = "Algorithmique et Programmation"; ects = 6 }
let _RCP110 = { code = "RCP110"; title = "Recherche opérationnelle et programmation linéaire avancée"; ects = 6 }
let _RCP207 = { code = "RCP207"; title = "Modélisation et Analyse de Systèmes Orientés Processus"; ects = 6 }
let _RCP208 = { code = "RCP208"; title = "Apprentissage statistique = modélisation descriptive et introduction aux réseaux de neurones"; ects = 6 }
let _RCP209 = { code = "RCP209"; title = "Apprentissage statistique = modélisation décisionnelle et apprentissage profond"; ects = 6 }
let _RCP216 = { code = "RCP216"; title = "Ingénierie de la fouille et de la visualisation de données massives"; ects = 6 }
let _RCP219 = { code = "RCP219"; title = "Outils mathématiques pour loptimisation numérique et combinatoire"; ects = 6 }
let _RCP220 = { code = "RCP220"; title = "Apprentissage statistique en production"; ects = 6 }
let _RSX101 = { code = "RSX101"; title = "Réseaux et protocoles pour l'Internet"; ects = 6 }
let _RSX102 = { code = "RSX102"; title = "Technologies pour les applications en réseau"; ects = 6 }
let _RSX103 = { code = "RSX103"; title = "Conception et urbanisation de services réseau"; ects = 6 }
let _RSX112 = { code = "RSX112"; title = "Sécurité des réseaux"; ects = 6 }
let _RSX116 = { code = "RSX116"; title = "Réseaux mobiles et sans fil"; ects = 6 }
let _RSX217 = { code = "RSX217"; title = "Nouvelles architectures de réseaux de communication"; ects = 6 }
let _RSX218 = { code = "RSX218"; title = "Projets avancés en réseaux"; ects = 6 }
let _RTC201 = { code = "RTC201"; title = "Socio-histoire de l'innovation techno-scientifique"; ects = 4 }
let _SEC101 = { code = "SEC101"; title = "Cybersécurité = référentiel; objectifs et déploiement"; ects = 6 }
let _SEC102 = { code = "SEC102"; title = "Menaces informatiques et codes malveillants = analyse et lutte"; ects = 6 }
let _SEC104 = { code = "SEC104"; title = "Analyse de risques des données; réseaux et systèmes"; ects = 6 }
let _SEC105 = { code = "SEC105"; title = "Architectures et bonnes pratiques de la sécurité des réseaux; des systèmes; des données et des applications"; ects = 6 }
let _SEC106 = { code = "SEC106"; title = "Analyses de sécurité = vulnérabilités et attaques"; ects = 6 }
let _SEC107 = { code = "SEC107"; title = "Conception d'architecture de sécurité à partir d'un audit de sécurité"; ects = 6 }
let _SEC108 = { code = "SEC108"; title = "Durcissement et mise en uvre de mesures de sécurité avancées pour les données; les réseaux et les systèmes (Hardening)"; ects = 6 }
let _SEC201 = { code = "SEC201"; title = "IAML = IA et du ML pour la cybersécurité"; ects = 6 }
let _SEC202 = { code = "SEC202"; title = "SACE Sécurité d'Architectures Complexes et Émergentes"; ects = 6 }
let _SMB101 = { code = "SMB101"; title = "Systèmes d'exploitation = principes; programmation et virtualisation"; ects = 6 }
let _SMB111 = { code = "SMB111"; title = "Systèmes et applications répartis pour le cloud"; ects = 6 }
let _SMB116 = { code = "SMB116"; title = "Conception et développement pour systèmes mobiles"; ects = 6 }
let _SMB214 = { code = "SMB214"; title = "Infrastructure technologique et nouveaux systèmes (1)"; ects = 6 }
let _SMB215 = { code = "SMB215"; title = "Infrastructure technologique et confiance (2)"; ects = 6 }
let _STA001 = { code = "STA001"; title = "Techniques de la statistique"; ects = 6 }
let _STA201 = { code = "STA201"; title = "Analyse multivariée approfondie"; ects = 9 }
let _STA211 = { code = "STA211"; title = "Entreposage et fouille de données"; ects = 9 }
let _TED001 = { code = "TED001"; title = "Enjeux des transitions écologiques= comprendre et agir"; ects = 3 }
let _TET102 = { code = "TET102"; title = "Management d'équipe et communication en entreprise"; ects = 6 }
let _UA2B30 = { code = "UA2B30"; title = "Test d'anglais"; ects = 0 }
let _UAAD91 = { code = "UAAD91"; title = "Examen d'admission à l'école d'ingénieur"; ects = 0 }
let _UAAL0S = { code = "UAAL0S"; title = "Expérience professionnelle"; ects = 50 }
let _UAAL0T = { code = "UAAL0T"; title = "Expérience professionnelle"; ects = 17 }
let _UAEP01 = { code = "UAEP01"; title = "Expérience professionnelle"; ects = 9 }
let _UAEP02 = { code = "UAEP02"; title = "Expérience professionnelle"; ects = 9 }
let _UAEP03 = { code = "UAEP03"; title = "Expérience professionnelle"; ects = 15 }
let _UAEP04 = { code = "UAEP04"; title = "Expérience professionnelle"; ects = 18 }
let _UAM91B = { code = "UAM91B"; title = "Mémoire ingénieur"; ects = 39 }
let _UAMM91 = { code = "UAMM91"; title = "Mémoire ingénieur"; ects = 42 }
let _UATN01 = { code = "UATN01"; title = "Activités liées à l'international"; ects = 3 }
let _UEU001 = { code = "UEU001"; title = "Union européenne = enjeux et grands débats"; ects = 4 }
let _UEU002 = { code = "UEU002"; title = "Mondialisation et Union européenne"; ects = 4 }
let _UTC501 = { code = "UTC501"; title = "Outils mathématiques pour Informatique"; ects = 3 }
let _UTC502 = { code = "UTC502"; title = "Principes fondamentaux des Systèmes d'exploitation"; ects = 3 }
let _UTC503 = { code = "UTC503"; title = "Paradigmes de programmation"; ects = 3 }
let _UTC504 = { code = "UTC504"; title = "Systèmes d'Information et Bases de Données"; ects = 3 }
let _UTC505 = { code = "UTC505"; title = "Introduction à la cyberstructure de l'internet = réseaux et sécurité"; ects = 3 }

let _LG02501A =
  { code = "LG02501A"
  ; name = "Licence informatique"
  ; curriculum =
      [ { ects = 15; units = [ _UTC501; _UTC502; _UTC503; _UTC504; _UTC505 ]; required_children = 0; children = [] }
      ; { ects = 18; units = []; required_children = 3; children =
            [ { ects = 6; units = [ _NFP119; _NFP121; _GLG105; _NSY115 ]; required_children = 0; children = [] }
            ; { ects = 6; units = [ _NFE108; _NFP107; _SEC101; _NFE114 ]; required_children = 0; children = [] }
            ; { ects = 6; units = [ _NSY103; _NSY104; _NSY014; _SEC102; _SMB101 ]; required_children = 0; children = [] }
            ; { ects = 6; units = [ _RSX101; _RSX102; _MUX101; _SEC105 ]; required_children = 0; children = [] }
            ; { ects = 6; units = [ _RCP105; _RCP101 ]; required_children = 0; children = [] }
            ]
        }
      ; { ects = 10; units = [ _GDN100; _ANG320 ]; required_children = 0; children = [] }
      ; { ects = 17; units = []; required_children = 1; children = 
        [ { ects = 50; units = [ _UAAL0S ]; required_children = 0; children = [] }
        ; { ects = 17; units = [ _UAAL0T ]; required_children = 0; children = [] }
        ]
        }
      ]
  }
;;
