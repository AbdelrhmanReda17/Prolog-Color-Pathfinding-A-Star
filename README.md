# Prolog Color Pathfinding using A*
This Prolog program utilizes the A* search algorithm to solve a path finding on a colored board. Given a board consisting of cells with colors (Red, Yellow, or Blue), the program searches for a path between a start and a goal cell of the same color.

### Requirements
Prolog interpreter (e.g., SWI-Prolog)

### Usage
1. Clone the repository to your local machine:
  ```git clone https://github.com/AbdelrhmanReda17/prolog-color-pathfinding-a-star.git```
2. Navigate to the project directory:
```cd prolog-color-pathfinding-a-star```
3. Run the Prolog program:
```swipl get-path.pl```
4. The program will output either a path found or a message indicating that no path exist.


### More Detailed : 
![image](https://github.com/AbdelrhmanReda17/Prolog-Color-Pathfinding-A-Star/assets/90706154/986e0f80-92c9-482a-ab0d-c007c7b96b3e)
- For example, as shown in the picture, 0,0 is the start cell and 1,3 is the end cell. The correct path is 0,0 -> 1,0 -> 2,0 -> 2,1 -> 2,2 -> 1,2 -> 1,3


- Example Input: ``?- find_path([[red, red , yellow ,yellow], [red, blue, red , red], [red, red , red, yellow] ,[blue, red , blue, yellow]] , [0,0] , [1,3]). ``
- Example Output: ```Path found: [[0,0],[1,0],[2,0],[2,1],[2,2],[1,2],[1,3]]```
