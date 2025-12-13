package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Task04 {
    private record Point (int y, int x) {
    }

    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t04.txt"));
        char[][] map = convert(lines);
        System.out.println(part2(map));
    }

    private static long part1(char[][] map) {
        return getAccessible(map).size();
    }

    private static long part2(char[][] map) {
        int before = getCountRolls(map);
        while (access(map)) {};
        return before - getCountRolls(map);
    }

    private static int getCountRolls(char[][] map) {
        int count = 0;
        for (int y = 0; y < map.length; y++) {
            for (int x = 0; x < map[y].length; x++) {
                if (map[y][x] == '@') {
                    count++;
                }
            }
        }
        return count;
    }

    private static List<Point> getAccessible(char[][] map) {
        var points = new ArrayList<Point>();
        for (int y = 0; y < map.length; y++) {
            for (int x = 0; x < map[y].length; x++) {
                if (map[y][x] == '@') {
                    if (getNeighborCount(map, new Point(y, x)) < 4) {
                        points.add(new Point(y, x));
                    }
                }
            }
        }
        return points;
    }

    private static char[][] convert(List<String> lines) {
        char[][] map = new char[lines.size()][];
        for  (int i = 0; i < lines.size(); i++) {
            map[i] = lines.get(i).toCharArray();
        }
        return map;
    }

    private static int getNeighborCount(char[][] map, Point p) {
        int[] dirs = new int[] {-1, 0, 1};
        int count = 0;
        for (int x : dirs) {
            for (int y : dirs) {
                if (x != 0 || y != 0 ) {
                    int nx = p.x + x;
                    int ny = p.y + y;
                    if (ny >= 0 && ny < map.length && nx >= 0 && nx < map[ny].length) {
                        if (map[ny][nx] == '@') {
                            count++;
                        }
                    }
                }
            }
        }
        return count;
    }

    private static boolean access(char[][] map) {
        var points = getAccessible(map);
        if (points.isEmpty()) {
            return false;
        }
        for (Point p : points) {
            map[p.y][p.x] = '.';
        }
        return true;
    }
}
