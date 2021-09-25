   

-   **Ok boomer (1/2), il n'y a pas de changement climatique !**

Dans **Debtwatch**, rien ne vous empêche de modifier la croissance potentielle (perte de productivité, contrainte sur l'activité), les besoins en dépenses publiques (compensation, investissement) ou une hausse des prélèvements obligatoires (socialisation des coûts de la transition, taxation comportementale). L'articulation de ces différentes hypothèses vous appartient, sachant qu'il y a de nombreux scénarios de transition ou de dommages climatiques envisageables. La seule difficulté est de faire "entrer" un scénario de changement climatique dans les paramètres plutôt abstraits de **Debtwatch**.

-   **Ok, boomer (2/2), jamais entendu parler de la MMT et de l'annulation de la dette publique ?**

C'est vrai, dans **Debtwatch**, la dette publique ne s'annule pas. Le modèle sert à calculer l'évolution de la dette et nous nous en sommes tenu là. En revanche, il est possible de simuler une détention perpétuelle par la Banque Centrale, qui réduit les intêrêts nets payés sur la dette par le gouvernement. Attention à bien préciser le taux des titres détenus par la Banque Centrale. Si ce dernier est proche de 0, l'effet sur la charge d'intérêt est limité.

-   **C'est un modèle keynésien (c'est l'OFCE non ?), et il ne peut que pousser à une augmentation des dépenses, alors...**

Dans **Debtwatch**, il est facile de neutraliser les multiplicateurs budgétaires, bien que la littérature économique soit aujourd'hui claire sur la validité empirique des multiplicateurs budgétaires (l'impact d'une hausse de la dépense publique ou de la baisse des prélèvements sur l'activité) non nuls, au moins dans le court terme. Avec des multiplicateurs nuls, beaucoup des conclusions du modèle sont conservées. Vous pourrez aussi explorer comment des multiplicateurs plus élevés viennent modifier ces conclusions et peut être écrire de nouvelles pages de la *Théorie générale de l'emploi, de l'intérêt et de la monnaie*.

-   **Comment puis-je faire une relance budgétaire ?**

Dans **Debtwatch**, il est facile d'augmenter de façon permanente les dépenses publiques et de relâcher la cible de dette publique. L'articulation de ce scénario avec l'écart taux croissance, la cible d'inflation ou le potentiel vous appartient. Il n'est pas possible pour le moment de définir une cible de dette à l'horizon de quelques années.

-   **Dans beaucoup de rapports, la dette explose, dans les simulations elle n'explose jamais. C'est un bug ?**

Dans **Debtwatch**, au lieu de postuler une politique inchangée pendant 10 ans (comme dans le rapport Arthuis par exemple), l'algortihme calcule les politiques nécessaires pour atteindre la cible de dette fixée. Pour certains paramètres, vous pourrez constater qu'il n'est pas possible d'atteindre une cible trop ambitieuse en 20 ans. Par ailleurs, le rapport Arthuis, disponible dans les scénarios prédéfinis, présente des simulations à 10 ans uniquement et ne permet pas donc de conclure à grand-chose sur les questions de dette à moyen terme. Formellement, sans contrôle (i.e. sans politique budgétaire calculée pour atteindre la cible) le système dynamique d'accumulation de dette n'a presque toujours pas de solution stable. Avec la politique budgétaire, il en a presque toujours une.

-   **Dans ce modèle, la dette peut augmenter sans conséquence**

Dans **Debtwatch**, le taux instantané dépend de la dette publique. En accroissant le paramètre qui en définit la sensibilité \[r_dette\], il est possible de simuler un scénario à la Grecque où les taux augmentent fortement et où l'effet boule de neige obligerait à des efforts budgétaires considérables. En combinant avec l'écart taux croissance de long terme, on peut arriver à des scénarios catastrophes où la hausse de la dette induit des choix intenables en matière budgétaire.

-   **Je change la croissance potentielle \[gpot\], mais cela n'a pas beaucoup d'impact, pourquoi ?**

Dans **Debtwatch**, les dépenses publiques s'alignent sur le PIB en valeur à long terme. Si l'économie croit plus, nous supposons par exemple que les salaires seront plus élevés y compris dans le secteur public. Néanmoins vous pouvez combiner une réduction de la croissance et un augmentation de la part des dépenses publiques et en observer les conséquences. Par ailleurs, si la cible de dette est la même et, si cela fonctionne comme prévu, le ratio dette sur pib sera inchangé à terme !

-   **Je trouve que le taux d'intérêt et la charge de la dette n'augmentent pas très vite**

Dans **Debtwatch**, le taux souverain est une moyenne pondéré des taux sur les émissions passées. Parce que la maturité est plutôt longue, même si le taux de marché augmente, la transmission se fait lentement au taux moyen pondéré. En jouant sur \[r_mat\] il est possible d'accélérer cette transmission, même si le changement de ce paramètre n'est pas réaliste : il faudrait rembourser les obligations passées de façon anticipée et se réendetter avec des maturités courtes !

-   **Ca calcule vraiment vite, c'est sûr que ça fait tous les calculs ?**

Merci ! Dans l'algorithme de **Debtwatch**, on calcule d'abord les paramètres de la politique budgétaire et ensuite la simulation. Ces simulations sont répétées une grand nombre de fois en faisant des tirages aléatoires des perturbations. Grâce à un peu de magie noire (notamment la récursion du modèle dynamique est résolue) et aux outils que l'Imperial College a développé pour les simulations de modèles SIR pour le COVID depuis 2020, on arrive à réaliser toutes ces simulations en quelques secondes. Lorsque c'est possible, le résultat déjà calculé est réutilisé.

-   **Ca prend du temps de calculer une simulation, on peut pas accélérer tout ça ?**

Dans l'algorithme de **Debtwatch**, il y a beaucoup de calculs faits. Il est sans doute possible d'accélérer encore tout ça, notamment en réalisant la minimisation de la fonction de perte en C++ (ou en Julia) plutôt que partiellement en R. Si vous voulez contribuer, n'hésitez pas à nous rejoindre sur github.com/OFCE/dwr.

-   **Je reprends une simulation déjà calculée, mais je n'ai pas exactement les mêmes résultats ?**

Dans l'algorithme de **Debtwatch**, les simulations sont stochastiques. Pour des raisons d'efficacité d'une part et d'autres qui tiennent à la dynamique du système, d'autre part, la reproductibilité est limitée par le hasard. Néanmoins, sauf si vous avez des contre exemples, les simulations doivent être proches. Lorsque c'est possible, nous tirons aléatoirement le "seed" du générateur de nombres aléatoires et nous l'enregistrons avec les paramètres de la simulation. C'est ce "seed" qui est réemployé pour reproduire la simulation. 
