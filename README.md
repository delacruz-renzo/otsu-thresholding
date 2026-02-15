# Umbralización de Otsu (Otsu’s thresholding)

![R](https://img.shields.io/badge/R-4.4.1-blue?logo=r) ![terra](https://img.shields.io/badge/terra-1.8.60-green) ![sf](https://img.shields.io/badge/sf-1.0.16-brightgreen) ![RStoolbox](https://img.shields.io/badge/RStoolbox-0.3.1-orange) ![ggplot2](https://img.shields.io/badge/ggplot2-3.5.1-blueviolet) ![EBImage](https://img.shields.io/badge/EBImage-4.40.0-yellow)

**Autor**: Renzo Angel De La Cruz Gonzales

---

## Introducción

La segmentación de imágenes constituye una etapa fundamental en la visión por computadora, ya que permite transformar información continua en clases discretas con significado temático. En el ámbito de la teledetección (remote sensing), esta tarea adquiere especial relevancia, pues facilita la delimitación de coberturas terrestres como vegetación, cuerpos de agua o áreas antrópicas a partir de imágenes satelitales. En este sentido, la elección del umbral de separación suele ser determinante para la calidad del resultado. No obstante, la selección del umbral puede introducir subjetividad cuando se basa únicamente en valores reportados en la bibliografía o en criterios visuales, ya que dichos valores no son necesariamente transferibles entre sensores, resoluciones espaciales, condiciones atmosféricas o características propias de cada escena.

En este contexto, el método de umbralización de Otsu (Otsu’s thresholding) ofrece una alternativa estadística, reproducible y basada en datos para estimar automáticamente un umbral óptimo a partir de la distribución de intensidades de la imagen. Propuesto por Nobuyuki Otsu (1979), este método se fundamenta en maximizar la separación entre dos clases mediante la optimización de la varianza interclase, lo que permite obtener una segmentación binaria robusta bajo un marco probabilístico.

El objetivo de este proyecto es desarrollar el método de Otsu aplicado a un raster NDVI derivado de imágenes satelitales CBERS-4A sobre el Humedal de Santa Rosa. Para ello, se elaboro un flujo de trabajo que incluye la preparación de los datos, la discretización del índice en niveles de intensidad, el cálculo del umbral óptimo mediante una función implementada manualmente y la comparación de resultados con un método estándar (paquete *EBImage*). Finalmente, se presenta la segmentación binaria obtenida y se discuten aspectos prácticos como la influencia del rango de reescalamiento, las diferencias numéricas entre implementaciones y las limitaciones del método en escenarios donde el histograma no es aproximadamente bimodal.

---

📌 **Objetivos del Análisis**

- Explorar las características del dataset de viviendas en California.  
- Analizar correlaciones entre variables numéricas y categóricas. 
- Construir un modelo de **Regresión Lineal** para predecir el valor medio de las viviendas.
- Evaluar el desempeño del modelo con métricas como RMSE y R².
- Visualizar los resultados mediante gráficos estáticos y animaciones GIF comparando valores reales vs. predicciones.

---

🧪 **Archivo principal**

---

📓 Markdown:

---

📊 **Gráfico generado**  

<div style="text-align: center;">
  <img src="grafico.png" width="100%" />
</div>

---

💬 **Notas adicionales**

- Proyecto orientado a teledetección y segmentación de imágenes, con implementación paso a paso del método de Otsu aplicado a NDVI.
- Se incluye una implementación manual del algoritmo y una comparación con *EBImage* para validar resultados.

---

📚 **Desarrollado por Renz De La Cruz | Remote Sensing & GIS**

🔗 Sígueme en mis Redes Sociales: [linktr.ee/renzo-delacruz](https://www.linkedin.com/in/renzo-delacruz/)
