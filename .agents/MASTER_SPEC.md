Dana v0.2: Master Design Document
1. Filosofía y Núcleo
Dana es un lenguaje de programación orientado a grafos (GOP) diseñado para orquestación, middleware y sistemas distribuidos.

Separación Estricta: La lógica reside exclusivamente en los Nodos. La topología reside exclusivamente en los Grafos.

Identidad: No existen variables globales ni estado mutable implícito. Todo dato viaja encapsulado en Pulsos.

Ejecución: Basada en EPOS (Ephemeral Pulse Orchestration System). El flujo se controla mediante TraceIDs únicos por evento.

2. Especificación del Lenguaje (Gramática)
A. Definición de Nodos (node)
El átomo funcional. Define una interfaz estricta y un bloque de procesamiento.

Puertos: Se definen con in y out. Se permiten valores por defecto (= val).

Default Ports: Se puede marcar un puerto (uno por dirección) como default. Esto permite conexiones simplificadas en el grafo.

Process Block:

Acepta variables inmutables locales (let).

Utiliza Pattern Matching (match) para la toma de decisiones.

Emisión: Se invoca el nombre del puerto de salida como una función (port_name(data)).

Fragmento de código

node Router {
    // Puertos con 'default' para Syntactic Sugar
    default in req: HttpRequest
    
    out api: HttpRequest
    out static: String
    default out error: HttpRequest

    process(req) {
        // Variables inmutables permitidas para claridad
        let path = req.path
        let method = req.method

        // Pattern Matching interno
        match (path, method) {
            (p, _) if p.startsWith("/api") => api(req)
            ("/", "GET")                   => static("index.html")
            _                              => error(req)
        }
    }
}
B. Definición de Grafos (graph)
El plano de la topología. Es puramente declarativo.

Conexiones (->): Unidireccionales.

Referencia: Se usa el nombre completo Nodo.puerto.

Guards ([...]): Filtros booleanos en las aristas.

Prohibiciones: No hay if, match, let ni instanciación con alias (node X = Y) dentro del grafo.

C. Syntactic Sugar (Aprobado)
1. Inferencia de Puertos (Default Ports)
Si un nodo tiene puertos marcados como default, se puede omitir el nombre del puerto en la conexión.

NodeA -> NodeB equivale a NodeA.default_out -> NodeB.default_in.

2. Bloques de Puertos (Port Blocks)
Para conectar múltiples puertos de un mismo nodo sin repetir el nombre del nodo. Mejora la legibilidad y agrupación.

Sintaxis:

Fragmento de código

SourceNode {
    .portA -> Dest1
    .portB -> Dest2
}
3. Runtime: EPOS (Ephemeral Pulse Orchestration System)
El motor de ejecución en Rust se basa en el movimiento de datos, no en la residencia de procesos.

TraceID: Cada evento raíz genera un ID único. Este ID propaga el contexto a través del grafo, permitiendo recursión y concurrencia segura sin bloqueos.

Stateless Execution: Los nodos no guardan estado entre ejecuciones de process. Las variables let viven solo en el stack de la ejecución actual del pulso.

Context Store: Si se requiere estado persistente (ej: Collector, Join), se almacena en un mapa externo gestionado por el Runtime, indexado por TraceID.

4. Ejemplo Canónico: Reverse Proxy v0.2
Este ejemplo muestra todas las características aprobadas funcionando en armonía.

Fragmento de código

import "std/net" as Net
import "std/fs" as FS

// Nodos estáticos (Configuración)
node CONFIG {
    default out port: Int = 8080
    out api_url: String = "http://api-service"
}

// Lógica de enrutamiento encapsulada
node RouteLogic {
    default in req: HttpRequest
    
    out to_api: HttpRequest
    out to_static: String
    out to_404: HttpRequest

    process(req) {
        match req.path {
            p if p.startsWith("/api") => to_api(req)
            "/"                       => to_static("index.html")
            _                         => to_404(req)
        }
    }
}

graph ProxyMain {
    // 1. Configuración usando Default Port implícito en CONFIG
    CONFIG -> Net.Http.Server.port

    // 2. Flujo Principal
    // Conexión implícita: Server.request -> RouteLogic.req (ambos son default)
    Net.Http.Server -> RouteLogic

    // 3. Distribución usando PORT BLOCKS
    RouteLogic {
        // Rama API
        .to_api -> Net.Http.Client // Infiere Client.req (default)

        // Rama Estática
        .to_static -> FS.Read.path
        // Reutilizamos la señal para disparar la lectura
        .to_static -> FS.Read.trigger 

        // Rama Error
        .to_404 -> Net.Http.ErrorBuilder // Infiere ErrorBuilder.req (default)
    }

    // 4. Configuración de dependencias
    CONFIG.api_url -> Net.Http.Client.base_url
    200            -> Net.Http.Response.status
    404            -> Net.Http.ErrorBuilder.status

    // 5. Fan-In (Retorno al servidor)
    // Conexión explicita necesaria porque Server.reply NO es default in (el default in es request)
    Net.Http.Client.res       -> Net.Http.Server.reply
    
    // Encadenamiento FS -> Response -> Server
    FS.Read.content           -> Net.Http.Response.body
    Net.Http.Response.res     -> Net.Http.Server.reply
    
    Net.Http.ErrorBuilder.res -> Net.Http.Server.reply
}
5. Hoja de Ruta de Implementación (Next Steps)
Parser (Rust): Implementar gramática para node (con let/match) y graph (con {} blocks y default logic).

Graph Compiler: Lógica de validación que resuelva los default ports buscando en la definición del nodo y expanda los port blocks.

Runtime EPOS: Implementar el evaluador de Pattern Matching dentro del ciclo de vida del Worker.