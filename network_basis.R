install.packages('igraph') # 네트워크 생성 패키지.
library(igraph)

# 네트워크 만들기.
# 스타형.
G.star <- make_star(6, mode="undirected", center=1) %>%
  set_vertex_attr('name', value=c("A","B","C","D","E","F"))
plot(G.star, vertex.color=rainbow(6), vertex.size=60)
tkplot(G.star, vertex.color=rainbow(6), vertex.size=60) # tkplot()시, 하나의 창에 인터랙티브 그래프 출력.

# 원형.
G.ring <- make_ring(6, directed = F, circular = T) %>%
  set_vertex_attr('name', value=c("A","B","C","D","E","F"))
plot(G.ring, vertex.color=rainbow(6), vertex.size=60)
tkplot(G.ring, vertex.color=rainbow(6), vertex.size=60) # tkplot()시, 하나의 창에 인터랙티브 그래프 출력.

# Y자형.
G.Y <- make_graph(edge=NULL, n=NULL, directed = F) # 그래프 초기화.
G.Y <- G.Y + vertices("A","B","C","D","E","F") # 노드 추가.
G.Y <- G.Y + edges("A", "B", # 가지 추가(각 연결 노드를 짝으로 나열)
                   "A","C",
                   "A","D",
                   "D","E",
                   "E","F")
plot(G.Y, vertex.color=rainbow(6), vertex.size=60)
tkplot(G.star, vertex.color=rainbow(6), vertex.size=60) # tkplot()시, 하나의 창에 인터랙티브 그래프 출력.

# 스타형 연결정도(Degree)의 중심성과 중심화 계산.
degree(G.star, normalized = F) # 비정규형.
degree(G.star, normalized = T) # 정규형.
CD <- centralization.degree(G.star, normalized = F) # 중심화(비정규형)
CD # res는 Cd(i), centralization은 Cd(G), theoretical_max는 Td,max.

# 스타형 근접(Closeness) 중심성과 중심화 계산.
closeness(G.star, normalized = F) # 비정규형.
closeness(G.star, normalized = T) # 정규형.
CC <- centralization.closeness(G.star, normalized = F)
CC$centralization/(6-1) # igraph 패키지 오류로 (n-1)로 나눠주어야 함.
CC$theoretical_max/(6-1)
CC$centralization/CC$theoretical_max # 정규화된 근접 중심화. C'c(G).

# 스타형 중개(Betweeness) 중심성과 중심화 계산.
betweenness(G.star, nobigint = F) # 비정규형.
betweenness(G.star, nobigint = T) # 정규형.
CB <- centralization.betweenness(G.star, normalized = F) # 중개 중심화(비정규형).
CB

# 각 네트워크 밀도(Density)
graph.density(G.star) # 스타형. 
graph.density(G.ring) # 원형. 
graph.density(G.Y) # Y형. 

# 각 네트워크 최단경로(Geodesic path)와 경로.
shortest.paths(G.Y) # 각 노드 간 경로에 대한 거리.
distances(G.Y, v='A', to='E') # A노드로부터 E로 연결된 거리.
get.shortest.paths(G.Y, 'A', 'E')$vpath[[1]] # A노드로부터 E로 연결된 경로.
average.path.length(G.Y) # 네트워크 경로들에 대한 평균 거리.
