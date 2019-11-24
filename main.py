#! /usr/bin/Rscript
import subprocess
from flask import Flask, escape, request
app = Flask(__name__)

def writeFile(rfile):

   # rfiles=["Rscript --vanilla Stock-forecast.r > out.txt","Rscript --vanilla beta-of-single-stock.r > out1.txt",\
      # "Rscript --vanilla Beta-of-portfolio.r > out2.txt"]

    subprocess.call(rfile, shell=True)

    with open('/tmp/out.txt') as output:
        data = output.readlines()
        # print("data", data)

    data=[x.replace('[1]', '').replace('\n', '') for x in data]
    return data


def main():

    @app.route("/forecast")
    def forecast():
        data = writeFile("R_STOCK=" + request.args.get('stock') +" Rscript --vanilla Stock-forecast.r > /tmp/out.txt")
        return ", ".join(data)

    @app.route("/single")
    def single():
        data1 = writeFile("R_STOCK=" + request.args.get('stock') +" Rscript --vanilla beta-of-single-stock.r > /tmp/out.txt")
        return str(data1[0])

    @app.route("/portfolioresults")
    def portfolioresults():
        data3 = writeFile("R_RISK="+ request.args.get('risk') + " Rscript --vanilla portfolio-results.r > /tmp/out.txt")
        return ", ".join(data3)

main()

