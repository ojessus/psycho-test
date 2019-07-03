# psycho-test

Estamos buscando a alguien que pueda análizar datos de evaluaciones de miles de niños de forma casi automática utilizando MySQL, R y RMarkdown. En particular, modelamos la habilidad de los niños usando el modelo Rasch y el paquete TAM de R. No importa si no tienes experiencia en esto, mientras lo quieras y puedas aprender.

Lecturas recomendadas:

* https://swcarpentry.github.io/r-novice-gapminder/02-project-intro/ para crear proyectos en R reproducibles y autocontenidos.

* https://swcarpentry.github.io/git-novice/14-supplemental-rstudio/ para utilizar git.

* https://en.wikipedia.org/wiki/Rasch_model sobre el modelo de Rasch




En tres campus (Campus A, Campus B y Campus C) se aplicó un examen para medir habilidades lectoras de niños de 6o de primaria en los ciclos escolares (year) 2017-2018 (2018) y 2018-2019 (2019). El examen tiene 39 reactivos; cada reactivo (idQuestion) tiene una dificultad y discriminación distintas (por estimar, utilizando el model Rasch), y cada niño contestó (o dejó en blanco) correcta o incorrectamente estas preguntas.

A partir de los datos proporcionados, contestar:

1. En un data frame, ¿cuáles son los parámetros (dificultad y discriminación) de los ítems en 2018?

2. El ranking de las escuelas por nivel académico

3. El ranking de los alumnos dentro de una escuela por nivel de habilidad

4. Dados los parámetros del modelo 2018, ¿los alumnos 2019 mejoraron o empeoraron en general (todos los campus) y por campus (qué campus empeoró, qué campus mejoró, qué campus se quedó igual)?  

------

Welcome to ProjectTemplate!

This file introduces you to ProjectTemplate, but you should eventually replace
the contents of this file with an introduction to your project. People who
work with your data in the future will thank you for it, including your future
self.

ProjectTemplate is an R package that helps you organize your statistical
analysis projects. Since you're reading this file, we'll assume that you've
already called `create.project()` to set up this project and all of its
contents.

To load your new project, you'll first need to `setwd()` into the directory
where this README file is located (pro tip: use RStudio's projects - .Rproject ). Then you need to run the following two
lines of R code:

	library('ProjectTemplate')
	load.project()

After you enter the second line of code, you'll see a series of automated
messages as ProjectTemplate goes about doing its work. This work involves:
* Reading in the global configuration file contained in `config`.
* Loading any R packages you listed in the configuration file.
* Reading in any datasets stored in `data` or `cache`.
* Preprocessing your data using the files in the `munge` directory.

Once that's done, you can execute any code you'd like. For every analysis
you create, we'd recommend putting a separate file in the `src` directory.
If the files start with the two lines mentioned above:

	library('ProjectTemplate')
	load.project()

You'll have access to all of your data, already fully preprocessed, and
all of the libraries you want to use.

For more details about ProjectTemplate, see http://projecttemplate.net
