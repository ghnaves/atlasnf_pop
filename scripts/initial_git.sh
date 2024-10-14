rm -rf .git

#Inicializar o Repositório Local
git init
#Adicionar todos os arquivos do seu projeto para que fiquem prontos para o commit:
git add .

#salvar alterações no histórico do Git
git commit -m "First commit"

#===> Depois de criar o repositório no GitHub.com

git remote add origin git@github.com:ghnaves/atlasnf_pop.git

#Cerificar o Remote
git remote -v

#Enviar o repositório local para o repositório remoto
git push -u origin main
#OU
git push origin main --force

#Verificar o status do repositório
git status

#Se tiver arquivos não rastreados, adicione-os ao repositório
# ==> Changes not staged for commit:
git add .