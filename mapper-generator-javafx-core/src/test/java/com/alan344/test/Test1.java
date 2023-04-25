package com.alan344.test;

import cn.hutool.core.compiler.CompilerUtil;
import com.alan344.service.MybatisPluginService;

import java.io.File;

/**
 * @author AlanSun
 * @date 2023/4/26 1:50
 */
public class Test1 {

    public static void main(String[] args) throws ClassNotFoundException {
        final ClassLoader compile = CompilerUtil.getCompiler(MybatisPluginService.class.getClassLoader())
                .addSource(new File("D:\\data\\simple-data-test\\test-usual-project\\src\\main\\java\\com\\controller\\InspectionTaskConfigItemTemplateController.java"))
                .compile();
        final Class<?> aClass = compile.loadClass("InspectionTaskConfigItemTemplateController");
    }
}
