namespace DataStructures.Structures.Graph

module Graph =

    type Graph<'T> =
        {
            nodes: 'T list
            edges: ('T * 'T) list
        }

    let createGraph nodes edges =
        { nodes = nodes; edges = edges }

    let addEdge graph (src, dest) =
        { graph with edges = (src, dest) :: graph.edges }

    let getNeighbors graph node =
        graph.edges
        |> List.filter (fun (src, dest) -> src = node || dest = node)
        |> List.map (fun (src, dest) -> if src = node then dest else src)