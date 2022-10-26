# fichero: teoriadecision_funciones_incertidumbre_nuevo.R ----
## Funciones útiles ----

crea.tablaX = function(vector_matporfilas,numalternativas=3,numestados=4) {
  # valores de entrada:
  # matriz de valores de las decisiones según cada estado, se introduce por filas,
  # esto es, todos los de la primera alternativa, luego los de la segunda, etc.
  # segundo argumento: número de altenativas; tecer argumento: número de estados
  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numestados,byrow=TRUE)
  # una vez hemos creado la matriz correspondiente solo falta poner los nombres
  colnames(X) = paste('e',1:numestados,sep='');
  # las columnas son los estados del problema. Las filas son las alternativas
  # por defecto se numeran desde 1, siendo e los estados y d las decisiones (alternaticas)
  rownames(X) = paste('d',1:numalternativas,sep='');
  return(X);
  # valor de salida: matriz con los coeficientes introducidos y los nombres correspondientes
}

# Introducimos los datos en R en forma de matriz:
#   ```{r}
# X = matrix(c(5,4,6,2,3,1,-1,8,7,5,2,0),nrow=4,ncol=3,byrow=TRUE)
# X
# colnames(X)=c('e1','e2','e3')
# rownames(X)=c('d1','d2','d3','d4')
# X
# ```


# la entrada de esta función es un vector
# el valor de salida es un vector con las posiciones en que se encuentra
# el menor valor de dicho vector
which.min.general = function(vector) {
  minimo = min(vector);
  res = which(vector == minimo); 
  # el comando which dice cuál es la posición del mínimo en el vector.
  # si hay empate muestra todas las que hay, no sólo el primero.
  return(res);

}


# la entrada de esta función es un vector
# el valor de salida es un vector con las posiciones en que se encuentra
# el mayor valor de dicho vector
which.max.general = function(vector) {
  maximo = max(vector);
  res = which(vector == maximo); ##which devuelve la posición del máximo vector
  return(res);

}


##which.min.general(c(3,2,8,2,9,2))
##which.min.general(c(3,2,8,1,9,2))

# la entrada de esta función son dos vectores, entendidos como coordenadas de dos puntos
# el valor de salida es calcular la suma de cuadrados de diferencias entre sus coordenadas
# esto es, la distancia euclídea entendiendolas como puntos de R^n
distanciaEuclidea = function(pto1,pto2) {
  return( sqrt( sum( (pto1-pto2)^2 )  ) )
  # dados dos vectores (o puntos en R^n) primero hace la resta 
  # componente a componente, después el cuadrado de las diferencias,
  # hace la suma de esto, quedando entonces la suma de diferencias
  # al cuadrado. Por último lo que devuelve es la raíz cuadrada
  # de esto, que es la definición de la distancia Euclídea entre dos puntos
}

## Nota Magdalena, posible error en la función por el funcionamiento interno de R

# es razonable pensar que si utilizamos esta función sea para puntos en un mismo espacio
# esto es, con el mismo número de coordenadas. Y si se nos olvida una coma o una de las
# coordenadas la función sabría detectarlo. Veamos un ejemplo
 
# > distanciaEuclidea(c(1,2,3,4),c(3,5,4))
# [1] 3.872983
# Warning message:
#   In pto1 - pto2 :
#   longer object length is not a multiple of shorter object length

# vemos que calcula la distancia haciendo que el segundo vector sea c(3,5,4,3)
# y nos da un error

# pero ese error no aparece si se cumple que la longitud de uno de los vectores
# es múltiplo de la longitud del otro, veamos un ejemplo

# > distanciaEuclidea(c(1,2),c(5,7,4,2))
# [1] 7.071068

# vemos que aplica la distancia como si el vector primero fuese c(1,2,1,2)
# pero esto no es lo que le habíamos pedido a la función

# versión alternativa de la función
# aunque para lo que la usamos en este script no tenga influencia porque
# las tablas siempre trabajan con el mismo número de coordenadas (estados)
# en cada vector (alternativa) para un uso distinto vendría bien esta versión

distanciaEuclidea2 = function(pto1,pto2) {
  if(length(pto1)==length(pto2)){
    return( sqrt( sum( (pto1-pto2)^2 )  ) )
  }
  # no hace falta un else porque si se cumple la condición sale del bucle
  warning("Los vectores introducidos tienen distinto número de coordenadas")
}


# la entrada a esta función debe ser un número, o al menos un único valor
# para los valores 2 y 3 tiene tablas predefinidas
# para el resto de valores tiene definida la tabla correspondiente al valor 1
# en cualquier caso la salida es una tabla creada según la función crea.tablaX
# definida al inicio de este script
criterio.tablaX.ejemplos = function(cual=1) {

  if (cual==2) { ## cual == 2  ## desfav.
    X = crea.tablaX(c(2,12,-3,5,5,-1,0,10,-2),numalternativas = 3,numestados = 3)
  } else if (cual==3) { ## cual == 3  ## desfav.
    X = crea.tablaX(c(125,120,156,60,130,80),numalternativas = 3,numestados = 2)
  } else {  ## cual == 1
    X = crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0),numalternativas = 4,numestados = 3)
  }
  return(X);

}

# podemos ver que la función también da tabla 1 como salida para cualquier
# número distinto de 2 o 3, y también si introducimos una cadena de texto

# > criterio.tablaX.ejemplos("texto")
# e1 e2 e3
# d1  5  4  6
# d2  2  3  1
# d3 -1  8  7
# d4  5  2  0

# pero en cambio da un error si introducimos un vector, pues no puede
# aplicar los condicionales de forma correcta

# > criterio.tablaX.ejemplos(1:3)
# Error in if (cual == 2) { : the condition has length > 1
  

## Funciones Métodos de Decisión bajo Incertidumbre ----

## Criterio de Wald o Pesimista

 # Para cada alternativa se supone que va a pasar lo peor y elige la alternativa 
 # que dé mejor valor. Nos aseguramos que en el peor de los casos obtengamos lo mejor 
 # posible, por tanto es una visión pesimista.
criterio.Wald = function(tablaX,favorable=TRUE) {
  # Entrada: tabla de las alternativas y estados con los coeficientes.
  #          método = (T/F).
  X = tablaX; #renombramos la tabla
  if (favorable) {
    AltW = apply(X,MARGIN=1,min);
    # Calcula el mínimo de nuestra tabla por filas, es decir, por alternativa, porque hablamos de ganancias.
    ##AltW
    Wald = max(AltW);
    # Calcula el máximo de los mínimos calculados. Queremos maximizar la ganancia.
    Alt_Wald = which.max.general(AltW);
    # Aquí vemos en qué posición se alcanza el máximo a través de which.max.general (explicada anteriormente).
    metodo = 'favorable';
    # Nos encontramos en el método favorable.
  } else {
    AltW = apply(X,MARGIN=1,max);
    # Ahora hablamos de costes, luego queremos maximizar lo que tendríamos que pagar por alternativa.
    ##AltW
    Wald = min(AltW);
    # De este modo, minizamos el coste a pagar.
    Alt_Wald = which.min.general(AltW);
    # Obtenemos la posición donde se alcanza el mínimo.
    metodo = 'desfavorable';
  }
  resultados = list();
  # Creamos una lista con el nombre del criterio, el método (si es favorable o no favorable),
  # la tabla con los datos, el mínimo o máximo por alternativa (dependiendo del método),
  # el valor óptimo que será el máximo de los mínimos(maxmin) o el mínimo de los máximos(minmax).
  # Por último, nos muestra las decisiones óptimas.
  resultados$criterio = 'Wald';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = AltW;
  resultados$ValorOptimo = Wald;
  resultados$AlternativaOptima = Alt_Wald;

  return(resultados);
  # Devuelve la lista.

}





criterio.Optimista = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    AltM = apply(X,MARGIN=1,max);
    ##AltM
    Maximax = max(AltM);
    Alt_Maximax = which.max.general(AltM);
    metodo = 'favorable';
  } else {
    AltM = apply(X,MARGIN=1,min);
    ##AltM
    Maximax = min(AltM);
    Alt_Maximax = which.min.general(AltM);
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Optimista';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = AltM;
  resultados$ValorOptimo = Maximax;
  resultados$AlternativaOptima = Alt_Maximax;

  return(resultados);


}



## factor de optimismo   (alfab * "lo mejor" Altmax en favor. y Altmin en desf.)
criterio.Hurwicz = function(tablaX,alfa=0.3,favorable=TRUE) {
  # alfa es un escalar entre 0 y 1 para el que calcularemos la valoración
  X = tablaX;
  if (favorable) {
    # calculamos el mínimo y el máximo de cada decisión igual que en los
    # métodos de Wald y optimista
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    AltH = alfa * Altmax + (1-alfa) * Altmin
    # buscamos un equilibrio entre ambos, el alfa nos indica cuánta 
    # importancia le damos al mejor caso posible de cada decisión
    # y 1-alfa sería el peso del peor caso posible de la decisión
    # esta combinación es la que queremos maximizar
    Hurwicz = max(AltH)
    Alt_Hurwicz = which.max.general(AltH)
    # buscamos aquellas elecciones con el mayor valor posible
    # de combinación según alfa entre optimista y pesimista 
    metodo = 'favorable';
  } else {
    # en caso de ser desfavorable, el mejor caso es minimizar pérdidas
    # de modo que lo que multiplica alfa es el menor valor de la decisión
    # y 1-alfa el mayor valor de pérdidas de cada decisión (pesimista)
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    AltH = (1-alfa) * Altmax + alfa * Altmin
    # una vez obtenemos para cada decisión esa combinación de costes
    # lo que queremos es minimizarla, y por tanto calculamos el valor
    # mínimo y vemos en qué decisión o decisiones se alcanza
    Hurwicz = min(AltH)
    Alt_Hurwicz = which.min.general(AltH)
    metodo = 'desfavorable';
  }
  # con toda la información almacenada hacemos una tabla, en la que se indica
  # el nombre del método, el alfa que usamos, si era favorable o desfavorable,
  # la matriz de costes o de beneficios utilizada, el valor que cada decisión
  # tenía para la combinación de "alfa*optimista + (1-alfa)*pesimista"
  # el óptimo de dicha combinación según fueran costes o beneficios
  # y finalmente las decisiones que alcanzaron dicho óptimo
  resultados = list();
  resultados$criterio = 'Hurwicz';
  resultados$alfa = alfa;
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = AltH;
  resultados$ValorOptimo = Hurwicz;
  resultados$AlternativaOptima = Alt_Hurwicz;

  return(resultados);


}

## factor de optimismo   (alfab * "lo mejor" Altmax en favor. y Altmin en desf.)
criterio.Hurwicz.General = function(tablaX,alfa=0.3,favorable=TRUE) {
  # si alfa es un escalar entre 0 y 1 lo obtiene para ese único valor
  # si alfa es igual a un número mayor que 1, lo usa para obtener cálculos para dividir el rango 0-1
  X = tablaX;
  if (favorable) {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    if (alfa<=1) {
      valfa = c(alfa);
    } else {
      valfa = seq(from=0,to=1,by=(1/alfa)); ## alfa: 100, 200,
    }
    vHurwicz = rep(0,length(valfa))
    Alt_vHurwicz = rep(0,length(valfa))
    for (i in 1:length(valfa)) {
      alfab = valfa[i];
      vAltH = alfab * Altmax + (1-alfab) * Altmin;
      vHurwicz[i] = max(vAltH);
      Alt_vHurwicz[i] = which.max(vAltH);
      Alt_vHurwicz_g = which.max.general(vAltH);
    }
    metodo = 'favorable';
  } else {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    if (alfa<=1) {
      valfa = c(alfa);
    } else {
      valfa = seq(from=0,to=1,by=(1/alfa)); ## alfa: 100, 200,
    }
    vHurwicz = rep(0,length(valfa))
    Alt_vHurwicz = rep(0,length(valfa))
    for (i in 1:length(valfa)) {
      alfab = valfa[i];
      vAltH = (1-alfab) * Altmax + alfab * Altmin;
      vHurwicz[i] = min(vAltH);
      Alt_vHurwicz[i] = which.min(vAltH);
      Alt_vHurwicz_g = which.min.general(vAltH);

    }
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Hurwicz';
  resultados$alfa = alfa;
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = vAltH;
  resultados$ValorOptimo = vHurwicz;
  if (length(valfa)==1) {
    resultados$AlternativaOptima = Alt_vHurwicz_g;
  } else {
    resultados$AlternativaOptima = Alt_vHurwicz;
  }

  return(resultados);



}



dibuja.criterio.Hurwicz = function(tablaX,favorable=TRUE) {
  X = tablaX;
  Altmin = apply(X,MARGIN=1,min);
  Altmax = apply(X,MARGIN=1,max);
  valfa = seq(from=0,to=1,by=0.05);
  vHurwicz = rep(0,length(valfa));
  Alt_vHurwicz = rep(0,length(valfa));
  for (i in 1:length(valfa)) {
    alfab = valfa[i];
    if (favorable) {
      vAltH = alfab * Altmax + (1-alfab) * Altmin;
      vHurwicz[i] = max(vAltH)
    } else {
      vAltH = alfab * Altmin + (1-alfab) * Altmax;
      vHurwicz[i] = min(vAltH)
    }

  }

  x0=0;x1=1;
  y0 = min(Altmin);
  y1 = max(Altmax);
  rg = y1-y0;
  y0=y0-0.1*rg;y1=y1+0.1*rg;
  plot(c(x0,x1), c(y0,y1), type = "n", xlab = "alpha", ylab = "Criterio Hurwicz");
  nn = length(Altmin);
  colores = rainbow(nn);
  abline(v=0);
  abline(v=1);
  if (favorable) {
    for (i in 1:nn) {
      aa = Altmin[i];
      bb = (Altmax[i] - Altmin[i]);
      abline(a=aa,b=bb,col=colores[i]);
    }
  } else {
    for (i in 1:nn) {
      aa = Altmax[i];
      bb = (Altmin[i] - Altmax[i]);
      abline(a=aa,b=bb,col=colores[i]);
    }
  }
  lines(valfa,vHurwicz,col=rainbow(nn+1)[nn+1],lty=3,lwd=3)
  if (favorable) {
    legend("bottomright",legend=rownames(X),fill=colores,inset=0.05)
    title("Criterio de Hurwicz (favorable - línea discontinua)")
  } else {
    legend("topright",legend=rownames(X),fill=colores,inset=0.05)
    title("Criterio de Hurwicz (desfavorable - línea discontinua)")
  }

}


# FUNCION : esta funcion nos da los valores de alfa para los que las alternativas cambian

# Entrada: Tabla, favorable (T/F)
# Salida: Intervalo -> Alternativa (óptima para ese intervalo de alfa)
# Autores: Ana Solis, Luca Ricardi y Paula Gutiérrez (Noviembre-2021)

dibuja.criterio.Hurwicz_Intervalos = function(tablaX,favorable=TRUE,mostrarGrafico=TRUE) {
    X = tablaX # renombramos la tabla
    Altmin = apply(X,MARGIN=1,min)      # vector de minimos (por filas)
    Altmax = apply(X,MARGIN=1,max)      # vector de maximos (por filas)
    valfa = seq(from=0,to=1,by=0.05)    # vector de valores para alfa (entre 0 y 1)
    Hurw <- data.frame(Alt_opt = rep(0,length(valfa)),vHurwicz = rep(0,length(valfa)))

    #Alt_opt = rep(0,length(valfa))      # creamos el vector de decisiones (por el criterio de Hurwicz) para cada valor de alfa

    alfaCorte=c()                       # vector que contiene los valores de alfa donde cambian las decisiones
    for (i in 1:length(valfa)) {
        Opt <- criterio.Hurwicz(X, alfa = valfa[i], favorable)
        Hurw[i,] <-  rbind(Opt$AlternativaOptima[[1]],Opt$ValorOptimo) # obtenemos las alternativas para cada alfa
        Alt=c() # Este va a ser el Vector de las alternativas optimas para todos los alfa
        for (i in 1:dim(Hurw)[1]) {
            valrepetidos = duplicated(Hurw$Alt_opt) # Vector de TRUE/FALSE donde los FALSE son los elementos que se repiten
            if (isFALSE(valrepetidos[i])){
                Alt = c(Alt,Hurw$Alt_opt[i]) # Si es falso (si el valor se repite) lo almacenamos en el vector Alt
            }
        }
    }
    # Teniendo el vector de alternativas (Alt) buscamos los puntos de corte de las rectas asociadas a cada alternativa (beneficios)
    # Por ejemplo, la recta que sale de la alternativa a1 y a2 seria:
    #
    #               a1Max *alfa +(1-alfa)*a1Min = a2Max *alfa +(1-alfa)*a2Min
    #
    # Pasando todo a un  miembro e igualando a 0 nos queda:
    #
    #               alfa * (a1Max- a2Max - a1Min + a2Min) + a1Min -a2Min = 0
    #
    # Buscamos ahora los valores de alfa para los que se cortan las rectas asociadas a cada decision
    for (i in 1:(length(Alt)-1)){
        imax = as.numeric(Altmax[Alt[i]])      # maximo asociado a la decision i del vector Alt
        imax1 = as.numeric(Altmax[Alt[i+1]])   # maximo asociado a la decision i+1 del vector Alt
        imin = as.numeric(Altmin[Alt[i]])      # minimo asociado a la decision i del vector Alt
        imin1 = as.numeric(Altmin[Alt[i+1]])   # minimo asociado a la decision i+1 del vector Alt
        if (favorable){
            pCorte = function(alfa) {alfa * (imax-imax1-imin+imin1)+imin-imin1}
            alfaC = uniroot(pCorte, interval = c(0,1))$root[[1]] # Buscamos los 0 para cada funcion
            alfaCorte[i] = alfaC  # Almacenamos los valores de alfa para los que las rectas se cortan en alfaCorte
        } else {
            # Para el caso de costes (alternativas a1 y a2):
            #
            #               a1Max *(1-alfa) +alfa*a1Min = a2Max *(1-alfa) +alfa*a2Min
            #
            # Pasando todo a un  miembro e igualando a 0 nos queda:
            #
            #               alfa * (a1Min- a2Min - a1Max + a2Max) + a1Max -a2Max = 0
            #
            pCorte = function(alfa) {alfa * (imin-imin1-imax+imax1)+imax-imax1}
            alfaC = uniroot(pCorte, interval = c(0,1))$root[[1]] # Buscamos los 0 para cada funcion
            alfaCorte[i] = alfaC  # Almacenamos los valores de alfa para los que las rectas se cortan en alfaCorte
        }

    }

    if (mostrarGrafico) {
        x0=0;x1=1;
        y0 = min(Altmin);
        y1 = max(Altmax);
        rg = y1-y0;
        y0=y0-0.1*rg;y1=y1+0.1*rg;
        plot(c(x0,x1), c(y0,y1), type = "n", xlab = "alpha", ylab = "Criterio Hurwicz");
        nn = length(Altmin);
        colores = rainbow(nn) #aquí es donde estaba el fallo, por lo que salían todas las lineas azules.
        abline(v=0);
        abline(v=1);
        if (favorable) {
            for (i in 1:nn) {
                aa = Altmin[i];
                bb = (Altmax[i] - Altmin[i]);
                abline(a=aa,b=bb,col=colores[i]);
            }
        } else {
            for (i in 1:nn) {
                aa = Altmax[i];
                bb = (Altmin[i] - Altmax[i]);
                abline(a=aa,b=bb,col=colores[i]);
            }
        }

        lines(valfa,Hurw$vHurwicz,col="green",lty=3,lwd=3)
        abline(v = alfaCorte, col="red")

        if (favorable) {
            legend("bottomright",legend=rownames(X),fill=colores,inset=0.05) #leyendas añadidas
            title("Criterio de Hurwicz (favorable - línea discontinua)")
        } else {
            legend("topright",legend=rownames(X),fill=colores,inset=0.05) #leyendas añadidas
            title("Criterio de Hurwicz (desfavorable - línea discontinua)")
        }
    }

    alfaCorte = round(alfaCorte, 3)
    if (length(alfaCorte)==1){
        Int1=paste("(",0,",",alfaCorte,")")
        Int2=paste("(",alfaCorte,",",1,")")
        Soluciones = cbind(c(Int1,Int2),c(Alt[1],Alt[2]))
    } else {
        Int0=paste("(",0,",",alfaCorte[1],")")
        Int1=paste("(",alfaCorte[length(alfaCorte)],",",1,")")
        Int = ""
        Soluciones= c(Int0, Alt[1])
        for (i in 1:(length(alfaCorte)-1)){
            Int[i] = paste("(",alfaCorte[i],",",alfaCorte[i+1],")")
            Soluciones = rbind(Soluciones,c(Int[i],Alt[i+1]))
        }
        Soluciones = rbind(Soluciones,c(Int1,Alt[length(Alt)]))
    }
    colnames(Soluciones)=c("Intervalo","Alternativa")

    resultados = list();
    resultados$AltOptimas = Alt;
    resultados$PuntosDeCorte = alfaCorte;
    resultados$IntervalosAlfa = Soluciones;
    return(resultados)

}




## Savage

criterio.Savage = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    Mejores = apply(X,MARGIN=2,max);
    temp1 = rep(Mejores,dim(X)[1])
    Mmejores = matrix(temp1,nrow=dim(X)[1],ncol=dim(X)[2],byrow=TRUE);
    Pesos = abs(Mmejores-X);
    ##print(Pesos)
    ## Ahora criterio Wald Minimax Pesimista (desfavorable)
    AltWS= apply(Pesos,MARGIN=1,max);
    Savage = min(AltWS);
    Alt_Savage = which.min.general(AltWS);
    metodo = 'favorable';
  } else {
    Mejores = apply(X,MARGIN=2,min);
    temp1 = rep(Mejores,dim(X)[1])
    Mmejores = matrix(temp1,nrow=dim(X)[1],ncol=dim(X)[2],byrow=TRUE);
    Pesos = abs(Mmejores-X);
    ## Ahora criterio Wald Minimax (desfavorable)
    AltWS= apply(Pesos,MARGIN=1,max);
    Savage = min(AltWS);
    Alt_Savage = which.min.general(AltWS);
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Savage';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$Mejores = Mejores;
  resultados$Pesos = Pesos;
  resultados$ValorAlternativas = AltWS;
  resultados$ValorOptimo = Savage;
  resultados$AlternativaOptima = Alt_Savage;

  return(resultados);


}



criterio.Laplace = function(tablaX,favorable=TRUE) {
  # entrada: tabla con los valores de cada decisión y cada estado
  # método a utilizar: favorable (TRUE) o desfavorable (FALSE)
  # la tabla debe introducirse en un formato válido (con la función crea.tablaX)
  X = tablaX;
  # renombramos la tabla para mayor facilidad
  if (favorable) {
    AltL = apply(X,MARGIN=1,mean);
    # se hace la media de nuestra tabla por filas (por cada decisión, beneficio medio)
    Laplace = max(AltL) # favorable
    # como es el beneficio medio, queremos maximizarlo, lo guardamos en una variable
    Alt_Laplace = which.max.general(AltL)
    # vemos en qué posiciones se alcanza el máximo con la función auxiliar conveniente
    metodo = 'favorable';
  } else {
    AltL = apply(X,MARGIN=1,mean);
    # si el método no es favorable, hace también la media pero esta vez es del coste 
    Laplace = min(AltL) # desfavorable
    # queremos la elección que minimice el coste medio, almacenamos este valor
    Alt_Laplace = which.min.general(AltL)
    metodo = 'desfavorable';
    # de nuevo vemos dónde se alcanza dicho mínimo. Almacena también el método con el que estamos
  }
  resultados = list();
  # creamos una lista que será lo que devuelva nuestra función, en ella almacenamos
  # el nombre del criterio y el método (favorable o desfavorable), así como la tabla
  # con la que estamos trabajando y los valores de las medias por filas
  resultados$criterio = 'Laplace';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = AltL;
  resultados$ValorOptimo = Laplace;
  # aquí guardamos el valor óptimo, según el método será el máximo o mínimo de las medias
  
  resultados$AlternativaOptima = Alt_Laplace;
  # por último indicamos cuál o cuáles son las decisiones óptimas, y devolvemos la lista entera
  return(resultados);

}



criterio.PuntoIdeal = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    MejoresPT = apply(X,MARGIN=2,max); # favorable
    AltPT = rep(0,dim(X)[1])
    for (i in 1:dim(X)[1]) {
      AltPT[i] = distanciaEuclidea(MejoresPT,X[i,])
    }
    ##AltPT
    names(AltPT) = rownames(tablaX)
    PuntoIdeal = min(AltPT);
    Alt_PuntoIdeal = which.min.general(AltPT);
    metodo = 'favorable';
  } else {
    MejoresPT = apply(X,MARGIN=2,min); # desfavorable
    AltPT = rep(0,dim(X)[1])
    names(AltPT) = rownames(tablaX)
    for (i in 1:dim(X)[1]) {
      AltPT[i] = distanciaEuclidea(MejoresPT,X[i,])
    }
    ##AltPT
    PuntoIdeal = min(AltPT);
    Alt_PuntoIdeal = which.min.general(AltPT);
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Punto Ideal';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$Mejores = MejoresPT;
  resultados$ValorAlternativas = AltPT;
  resultados$ValorOptimo = PuntoIdeal;
  resultados$AlternativaOptima = Alt_PuntoIdeal;

  return(resultados);

}

criterio.Todos = function(tablaX,alfa=0.3,favorable=TRUE) {
  # Esta función sirve para mostrar todos los criterios con sus resultados a la vez 
  # en un mismo data.frame.
  # Entrada: tabla con los coeficientes de las alternativas y estados de la naturaleza.
  #          valor de alfa entre 0 y 1.
  #          método = T/F
  cri01 = criterio.Wald(tablaX,favorable);
  cri02 = criterio.Optimista(tablaX,favorable);
  cri03 = criterio.Hurwicz(tablaX,alfa,favorable);
  cri04 = criterio.Savage(tablaX,favorable);
  cri05 = criterio.Laplace(tablaX,favorable);
  cri06 = criterio.PuntoIdeal(tablaX,favorable);
  # Renombramos los criterios creados bajo incertidumbre con sus parámetros de entrada 
  # correspondientes.
  numestados = ncol(tablaX) # El número de columnas de la tabla se corresponde con el número de
  # estados de la naturaleza.
  numalterna = nrow(tablaX) # El número de filas es el número de alternativas.

  resultado = cbind(tablaX,cri01$ValorAlternativas,cri02$ValorAlternativas,
                    cri03$ValorAlternativas,cri04$ValorAlternativas,
                    cri05$ValorAlternativas,cri06$ValorAlternativas);
  
  # añadir la columna que cuente cuántas veces ha sido "óptima" cada decisión
  # primero unimos en un único vector todas las alternativas óptimas
  optimas=c(cri01$AlternativaOptima,cri02$AlternativaOptima,
            cri03$AlternativaOptima,cri04$AlternativaOptima,
            cri05$AlternativaOptima,cri06$AlternativaOptima)
  # hacemos un vector de ceros y contamos para cada decisión cuántas veces fue óptima
  Conteo=numeric(numalterna)
  for(i in 1:numalterna){
    Conteo[i]=sum(optimas==i)
  }
  # sobre escribo la tabla que antes era mi resultado
  resultado = cbind(resultado, Conteo)
  # se añade con ese nombre, si quisieramos cambiarlo podríamos hacerlo
  
  # juntamos por columnas la tabla creada y los valores de las alternativas por cada criterio
  # y lo guardamos.
  decopt = c(rep(NA,numestados),cri01$AlternativaOptima[1],
             cri02$AlternativaOptima[1],cri03$AlternativaOptima[1],
             cri04$AlternativaOptima[1],cri05$AlternativaOptima[1],
             cri06$AlternativaOptima[1],NA);
  # creamos un vector con repitiendo NA el número de estados que haya y la posición 1 de las 
  # alternativas óptimas de todos los criterios.
  resultado = rbind(resultado,decopt);
  # juntamos por filas el vector y lo agrupado con cbind. Lo guardamos.
  colnames(resultado)[numestados+1] = cri01$criterio;
  colnames(resultado)[numestados+2] = cri02$criterio;
  colnames(resultado)[numestados+3] = cri03$criterio;
  colnames(resultado)[numestados+4] = cri04$criterio;
  colnames(resultado)[numestados+5] = cri05$criterio;
  colnames(resultado)[numestados+6] = cri06$criterio;
  # Ponemos el nombre del criterio a las columnas del rbind obviando las numestados-primeras.

  if (favorable) {
    rownames(resultado)[numalterna+1] = 'iAlt.Opt (fav.)';
    # Para favorable = T, llamamos a la fila después de numalternativas como iAlt.Opt (fav.).
  } else {
    rownames(resultado)[numalterna+1] = 'iAlt.Opt (Desfav.)';
    # Para favorable = F, llamamos a la fila después de numalternativas como iAlt.Opt (Desfav.).
  }

  ## nuevo
  resultado = as.data.frame(resultado) # Transformamos el rbind en un data.frame.
  resultado = format(resultado,digits=4) # Redondeamos los resultados a 4 decimales.
  
  decopt = c(rep('--',numestados),
             paste0(names(cri01$AlternativaOptima),collapse = ","),
             paste0(names(cri02$AlternativaOptima),collapse = ","),
             paste0(names(cri03$AlternativaOptima),collapse = ","),
             paste0(names(cri04$AlternativaOptima),collapse = ","),
             paste0(names(cri05$AlternativaOptima),collapse = ","),
             paste0(names(cri06$AlternativaOptima),collapse = ","),
             paste0("d",which.max.general(Conteo)));
  # el último valor será la o las alternativas con mayor número de votos
  
  # Creamos un vector con -- numestados veces y con el comando paste0 obtenemos los nombres
  # de la alternativa óptima de cada criterio separadas con comas.
  resultado[nrow(resultado),] = decopt
  # Este vector lo utilizamos en el nombre de las filas del data.frame.
  ## fin nuevo

  return(resultado)

  #Devuelve el data.frame
}


