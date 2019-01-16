if (nrow(spark_installed_versions()) < 1) {
    spark_install(tail(spark_available_versions()$spark, n = 1))
}

sc <- spark_connect(master = "local")
spark_disconnect(sc)