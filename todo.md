# TODO

- [x] Mulighed for at gå frem og tilbage i bufferen med piletasterne
- [ ] Fiks terminal wrapping

- [ ] Regex på declare variabel navn
- [ ] Tilføj support for brackets rundt om variabel navne: echo ${variableName}and${othervariablename}

- [ ] Tests over hele linjen
- [x] Alias
- [x] Export
- [ ] SIGINT
- [x] Prompt konfig 
- [ ] Miljø, e.g. python til prompt
- [ ] Exit code visning hvis forrige kommando fejler
- [ ] Multi-line edits med \
- [x] Forbedret fejl håndtering, måske modellér med ADT's? overkill?
- [x] inline suggestions ligesom zsh-autosuggestions
- [ ] Fix tab autocompletions.
- [ ] Up/Down history navigation skal arbejde ud fra hvad der allerede står på linjen
- [ ] Eget scripting sprog?
- [x] Smartere pipes, f.eks. implementer filter og map kommandoer e.g., ls | filter(_.size > 10mb) | map(_.name). 
      Mangler stadig flere operationer: indtil videre filter, map på gt og eq
- [ ] Tilføj nogle flags til functionalOps: sort descending/ascending, map _.created, 
      accessed, modified skal kunne vælge imellem -t time, -d for date og ingen flags for datetime
- [ ] Tilføj standard flags til ls. Overvej at skjul hidden files ved ls bare
- [ ] Farve highlighted builtin keyword og externals, måske paths?
- [x] cat som en builtin så den kan bruges til dsl pipelines (Mangler flags)
- [x] grep?
- [ ] && og ||. bliver nødt til at refaktoere så exitcodes er bedre håndteret i executoren. måske ved en Result?

- [ ] ordentlig README
