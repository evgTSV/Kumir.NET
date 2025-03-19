Компилятор КуМир под CLI [![Status Enfer][status-enfer]][andivionian-status-classifier]
======
---
* CLI - .NET Common Language Infrastructure

Стандартная библиотека кумира лежит в `Kumir.NET.Compiler/stdlib`

### Разработка

---
* Стандартная библиотека - **Начато**
* Лексер - **Готов**
* Парсер - **Запланировано**
* Препроцессор - **Не начато**
* AST - **Не начато**
* Кодген - **Не начато** (Mono.Cecil)
* CLI (Console) - **Не начато**
* SDK - **Не начато**
* Шаблоны - **Не начато**

В планах переход на `Nuke` (Build system)

### Тесты:

---
Протестировать лексер: 
```sh
dotnet test Kumir.NET.Lexer.Tests
```

[status-enfer]: https://img.shields.io/badge/status-enfer-orange.svg
[andivionian-status-classifier]: https://github.com/ForNeVeR/andivionian-status-classifier#status-enfer-