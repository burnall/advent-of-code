package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

record Cmd(boolean clockwise, int clicks) {
};

public class Task01 {
    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t01.txt"));
        List<Cmd> cmds = parseCmds(lines);
        System.out.println(solve2(cmds));
    }

    private static long solve1(List<Cmd> cmds) {
        AtomicInteger pos = new AtomicInteger(50);
        return cmds.stream().map(cmd -> cmd.clicks() * (cmd.clockwise() ? 1 : -1))
                .map(pos::addAndGet)
                .filter(i -> i % 100 == 0)
                .count();
    }

    private static long solve2(List<Cmd> cmds) {
        AtomicInteger pos = new AtomicInteger(50);

        return cmds.stream().map(cmd -> cmd.clicks() * (cmd.clockwise() ? 1 : -1))
                .map(i-> {
                    int p = pos.get();
                    pos.set(mod100(p + i));
                    System.out.printf("%d %d %d\n", p, i, turns(p, i));
                    return turns(p, i);
                })
                .mapToLong(Integer::intValue)
                .sum();
    }

    private static List<Cmd> parseCmds(List<String> lines) {
        return lines.stream()
                .map(line -> new Cmd('R' == line.charAt(0),
                        Integer.parseInt(line.substring(1))))
                .toList();
    }

    private static int turns(int start, int clicks) {
        return clicks >= 0
                ? (start + clicks) / 100
                : ((start == 0 ? 0 : 100 - start) - clicks) / 100;
    }

    private static int mod100(int a) {
        return (100 + a % 100) % 100;
    }
}
