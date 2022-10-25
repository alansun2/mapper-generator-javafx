package com.alan344.config;

import com.zaxxer.hikari.HikariDataSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * @author AlanSun
 * @date 2022/10/25 14:35
 */
@Configuration(proxyBeanMethods = false)
public class JdbcConfig {

    @Bean
    public JdbcTemplate jdbcTemplate() {
        HikariDataSource dataSource = new HikariDataSource();
        JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
        return jdbcTemplate;
    }
}
