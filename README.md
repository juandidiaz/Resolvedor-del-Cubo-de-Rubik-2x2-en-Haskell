# Resolvedor del Cubo de Rubik 2x2 en Haskell

Este proyecto implementa un resolvedor para el cubo de Rubik 2x2 en el lenguaje Haskell. El código permite representar el cubo, realizar movimientos básicos y aplicar secuencias de movimientos para resolver el cubo.

## Características del Proyecto

- **Representación del Cubo**: Cada pieza y cara del cubo está representada como una estructura de datos, permitiendo modelar el cubo de Rubik 2x2 de manera programática.
- **Movimientos Básicos**: Incluye movimientos como giros horarios y antihorarios de cada cara del cubo.
- **Solución en Capas**: El algoritmo de solución sigue un enfoque en capas, resolviendo primero la cara blanca, luego la cara amarilla, y finalmente las capas del medio para alinear las piezas.

## Estructuras de Datos

- **Color**: Define los colores del cubo (`Blanco`, `Amarillo`, `Azul`, `Verde`, `Naranja`, `Rojo`).
- **Pieza**: Representa una pieza de un color específico.
- **Cara**: Representa una cara del cubo con cuatro piezas (`supIzq`, `infIzq`, `supDer`, `infDer`).
- **Cubo**: Representa el cubo completo con seis caras (`arriba`, `abajo`, `frente`, `atras`, `izquierda`, `derecha`).

## Movimientos

Los movimientos del cubo están representados mediante un tipo `Movimientos`, que incluye:

- Movimientos de cada cara en sentido horario y antihorario (`ArribaHorario`, `AbajoHorario`, `FrenteHorario`, etc.).
- Giros completos del cubo en distintas direcciones (`VueltaArriba`, `VueltaIzquierda`, etc.).

Cada movimiento cambia la disposición de las piezas en el cubo, permitiendo simular cualquier secuencia de movimientos.

## Resolución del Cubo

1. **Resolver Cara Blanca**: Primero se resuelve la cara blanca y se la coloca en la parte inferior.
2. **Resolver Cara Amarilla**: Luego, la cara amarilla se resuelve y se coloca en la parte superior.
3. **Resolver las Capas del Medio**: Finalmente, se alinean las capas del medio.

## Ejecución del Proyecto

Para resolver un cubo específico, puedes utilizar la función principal `proyecto`, que recibe un cubo y muestra paso a paso la solución, imprimiendo las secuencias de movimientos y el estado del cubo en cada paso.

## Ejemplo de Uso

Para ejecutar el resolvedor y resolver un cubo específico, define un cubo en el código y llama a la función `proyecto cubo`.

```haskell
-- Ejemplo de ejecución
main :: IO ()
main = do
    let cubo = cuboEjemplo -- Define un cubo con una configuración inicial
    proyecto cubo          -- Ejecuta el resolvedor
